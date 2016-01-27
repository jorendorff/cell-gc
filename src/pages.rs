///! Allocating pages of memory from the OS and carving them into individual allocations.

use std::{mem, ptr};
use std::marker::PhantomData;
use bit_vec::BitVec;
use traits::InHeap;
use heap::Heap;

/// Non-inlined function that serves as an entry point to marking. This is used
/// for marking root set entries.
unsafe fn mark_entry_point<'a, T: InHeap<'a>>(addr: *const ()) {
    InHeap::mark(&*(addr as *const T));
}

pub fn heap_type_id<'a, T: InHeap<'a>>() -> usize {
    mark_entry_point::<T> as *const () as usize
}

const PAGE_SIZE: usize = 0x1000;

/// We rely on all bits to the right of this bit being 0 in addresses of
/// TypedPage instances.
const PAGE_ALIGN: usize = 0x1000;

fn is_aligned(ptr: *const ()) -> bool {
    ptr as usize & (PAGE_ALIGN - 1) == 0
}

pub struct PageHeader<'a> {
    pub heap: *mut Heap<'a>,
    mark_bits: BitVec,
    allocated_bits: BitVec,
    mark_fn: unsafe fn(*const ()),
    sweep_fn: unsafe fn(&mut PageHeader<'a>),
    freelist: *mut ()
}

impl<'a> PageHeader<'a> {
    pub fn find(ptr: *mut ()) -> *mut PageHeader<'a> {
        let header_addr = ptr as usize & !(PAGE_ALIGN - 1);
        header_addr as *mut PageHeader<'a>
    }

    pub fn clear_mark_bits(&mut self) {
        self.mark_bits.clear();
    }

    /// True if nothing on this page is allocated.
    pub fn is_empty(&self) -> bool {
        self.allocated_bits.none()
    }

    pub unsafe fn mark(&self, ptr: *const ()) {
        (self.mark_fn)(ptr);
    }

    pub unsafe fn sweep(&mut self) {
        (self.sweep_fn)(self);
    }

    pub fn type_id(&self) -> usize {
        self.mark_fn as usize
    }

    pub fn downcast_mut<T: InHeap<'a>>(&mut self) -> Option<&mut TypedPage<'a, T>> {
        if heap_type_id::<T>() == self.type_id() {
            let ptr = self as *mut PageHeader<'a> as *mut TypedPage<'a, T>;
            Some(unsafe { &mut *ptr })
        } else {
            None
        }
    }
}

pub struct TypedPage<'a, T: InHeap<'a>> {
    pub header: PageHeader<'a>,
    pub allocations: PhantomData<T>
}

/// Returns the smallest multiple of `k` that is at least `n`.
///
/// Panics if the answer is too big to fit in a `usize`.
fn round_up(n: usize, k: usize) -> usize {
    let a = n / k * k;
    if a == n { n } else { n + k }
}


impl<'a, T: InHeap<'a>> TypedPage<'a, T> {
    /// Offset, in bytes, of the first allocation from the start of the page.
    fn first_allocation_offset() -> usize {
        round_up(mem::size_of::<PageHeader>(), mem::align_of::<T>())
    }

    /// Number of allocations that fit in a page.
    pub fn capacity() -> usize {
        (PAGE_SIZE - Self::first_allocation_offset()) / mem::size_of::<T>()
    }

    fn first_object_ptr(&self) -> *const T {
        ((self as *const Self as usize) + Self::first_allocation_offset()) as *const T
    }

    fn first_object_ptr_mut(&mut self) -> *mut T {
        ((self as *mut Self as usize) + Self::first_allocation_offset()) as *mut T
    }

    unsafe fn init(&mut self) {
        let mut p = self.first_object_ptr_mut();
        for _ in 0 .. Self::capacity() {
            self.add_to_free_list(p);
            p = p.offset(1);
        }
    }

    pub fn find(ptr: *const T) -> *mut TypedPage<'a, T> {
        let page_addr = ptr as usize & !(PAGE_ALIGN - 1);
        let page = page_addr as *mut TypedPage<'a, T>;
        assert_eq!(heap_type_id::<T>(), unsafe { (*page).header.type_id() });
        page
    }

    unsafe fn add_to_free_list(&mut self, p: *mut T) {
        let listp = p as *mut *mut ();
        *listp = self.header.freelist;
        assert_eq!(*listp, self.header.freelist);
        self.header.freelist = p as *mut ();
    }

    unsafe fn allocation_index(&self, ptr: *const T) -> usize {
        let base = self.first_object_ptr();

        // Check that ptr is in range.
        assert!(ptr >= base);
        assert!(ptr < base.offset(Self::capacity() as isize));

        let index = (ptr as usize - base as usize) / mem::size_of::<T>();
        assert_eq!(base.offset(index as isize), ptr);
        index
    }

    pub unsafe fn get_mark_bit(&mut self, ptr: *const T) -> bool {
        let index = self.allocation_index(ptr);
        self.header.mark_bits[index]
    }

    pub unsafe fn set_mark_bit(&mut self, ptr: *const T) {
        let index = self.allocation_index(ptr);
        self.header.mark_bits.set(index, true);
    }

    pub unsafe fn try_alloc(&mut self) -> Option<*mut T> {
        let p = self.header.freelist;
        if p.is_null() {
            None
        } else {
            let listp = p as *mut *mut ();
            self.header.freelist = *listp;
            let ap = p as *mut T;
            let index = self.allocation_index(ap);
            assert!(!self.header.allocated_bits[index]);
            self.header.allocated_bits.set(index, true);
            Some(ap)
        }
    }

    unsafe fn sweep(&mut self) {
        let mut p = self.first_object_ptr_mut();
        for i in 0 .. Self::capacity() {
            if self.header.allocated_bits[i] && !self.header.mark_bits[i] {
                // The next statement has the effect of calling p's destructor.
                ptr::read(p);
                self.header.allocated_bits.set(i, false);
                self.add_to_free_list(p);
            }
            p = p.offset(1);
        }
    }
}

unsafe fn sweep_entry_point<'a, T: InHeap<'a>>(header: &mut PageHeader<'a>) {
    header.downcast_mut::<T>().unwrap().sweep();
}

pub struct PageBox<'a>(*mut PageHeader<'a>);

impl<'a> PageBox<'a> {
    pub fn new<T: InHeap<'a>>(heap: *mut Heap<'a>) -> PageBox<'a> {
        assert!(mem::size_of::<TypedPage<'a, T>>() <= PAGE_SIZE);
        let raw_page: *mut () = {
            let mut vec: Vec<u8> = Vec::with_capacity(PAGE_SIZE);
            let page = vec.as_mut_ptr() as *mut ();

            // Rust makes no guarantee whatsoever that this will work.
            // If it doesn't, panic.
            assert!(is_aligned(page));

            mem::forget(vec);
            page
        };

        let page: *mut TypedPage<'a, T> = raw_page as *mut TypedPage<'a, T>;
        let capacity = TypedPage::<'a, T>::capacity();
        unsafe {
            ptr::write(page, TypedPage {
                header: PageHeader {
                    heap: heap,
                    mark_bits: BitVec::from_elem(capacity, false),
                    allocated_bits: BitVec::from_elem(capacity, false),
                    mark_fn: mark_entry_point::<T>,
                    sweep_fn: sweep_entry_point::<T>,
                    freelist: ptr::null_mut()
                },
                allocations: PhantomData
            });
            TypedPage::<'a, T>::init(&mut *page);
            PageBox(&mut (*page).header as *mut PageHeader<'a>)
        }
    }
}

impl<'a> ::std::ops::Deref for PageBox<'a> {
    type Target = PageHeader<'a>;
    fn deref(&self) -> &PageHeader<'a> {
        unsafe { &*self.0 }
    }
}

impl<'a> ::std::ops::DerefMut for PageBox<'a> {
    fn deref_mut(&mut self) -> &mut PageHeader<'a> {
        unsafe { &mut *self.0 }
    }
}

impl<'a> Drop for PageBox<'a> {
    fn drop(&mut self) {
        unsafe {
            assert!((*self.0).is_empty());

            // Call the header's destructor. (ptr::read returns a temporary copy of
            // the header. Since we don't store it anywhere, it is then dropped.)
            ptr::read(self.0);

            // Deallocate the memory.
            Vec::from_raw_parts(self.0 as *mut u8, 0, PAGE_SIZE);
        }
    }
}
