//! Allocating pages of memory from the OS and carving them into individual
//! allocations.

use std::{cmp, mem, ptr};
use std::marker::PhantomData;
use bit_vec::BitVec;
use traits::IntoHeap;
use heap::Heap;
use ptr::{Pointer, UntypedPointer};

/// Non-inlined function that serves as an entry point to marking. This is used
/// for marking root set entries.
unsafe fn mark_entry_point<'h, T: IntoHeap<'h>>(addr: UntypedPointer) {
    let addr = addr.as_typed_ptr::<T::In>();
    T::mark(addr.as_ref());
}

pub fn heap_type_id<'h, T: IntoHeap<'h>>() -> usize {
    mark_entry_point::<T> as *const () as usize
}

pub(crate) const PAGE_SIZE: usize = 0x1000;

/// We rely on all bits to the right of this bit being 0 in addresses of
/// TypedPage instances.
pub(crate) const PAGE_ALIGN: usize = 0x1000;

fn is_aligned(ptr: *const ()) -> bool {
    ptr as usize & (PAGE_ALIGN - 1) == 0
}

pub struct PageHeader {
    pub heap: *mut Heap,
    mark_bits: BitVec,
    allocated_bits: BitVec,
    mark_fn: unsafe fn(UntypedPointer),
    sweep_fn: unsafe fn(&mut PageHeader),
    freelist: *mut (),
}

impl PageHeader {
    pub fn find(ptr: UntypedPointer) -> *mut PageHeader {
        let header_addr = ptr.as_usize() & !(PAGE_ALIGN - 1);
        header_addr as *mut PageHeader
    }

    pub fn clear_mark_bits(&mut self) {
        self.mark_bits.clear();
    }

    /// True if nothing on this page is allocated.
    pub fn is_empty(&self) -> bool {
        self.allocated_bits.none()
    }

    pub unsafe fn mark(&self, ptr: UntypedPointer) {
        (self.mark_fn)(ptr);
    }

    pub unsafe fn sweep(&mut self) {
        (self.sweep_fn)(self);
    }

    pub fn type_id(&self) -> usize {
        self.mark_fn as usize
    }

    pub fn downcast_mut<'h, T: IntoHeap<'h>>(&mut self) -> Option<&mut TypedPage<T::In>> {
        if heap_type_id::<T>() == self.type_id() {
            let ptr = self as *mut PageHeader as *mut TypedPage<T::In>;
            Some(unsafe { &mut *ptr })
        } else {
            None
        }
    }
}

pub struct TypedPage<U> {
    pub header: PageHeader,
    pub allocations: PhantomData<U>,
}

/// Returns the smallest multiple of `k` that is at least `n`.
///
/// Panics if the answer is too big to fit in a `usize`.
fn round_up(n: usize, k: usize) -> usize {
    let a = n / k * k;
    if a == n { n } else { n + k }
}

impl<U> TypedPage<U> {
    /// The actual size of an allocation can't be smaller than the size of a
    /// pointer, due to the way we store the freelist by stealing a pointer
    /// from the allocation itself.
    fn allocation_size() -> usize {
        cmp::max(mem::size_of::<U>(), mem::size_of::<*mut U>())
    }

    fn allocation_align() -> usize {
        cmp::max(mem::align_of::<U>(), mem::align_of::<*mut U>())
    }

    /// Offset, in bytes, of the first allocation from the start of the page.
    pub(crate) fn first_allocation_offset() -> usize {
        round_up(mem::size_of::<PageHeader>(), Self::allocation_align())
    }

    /// Number of allocations that fit in a page.
    pub fn capacity() -> usize {
        (PAGE_SIZE - Self::first_allocation_offset()) / Self::allocation_size()
    }

    fn first_object_addr(&self) -> usize {
        (self as *const Self as usize) + Self::first_allocation_offset()
    }

    unsafe fn init(&mut self) {
        let mut addr = self.first_object_addr();
        for _ in 0..Self::capacity() {
            self.add_to_free_list(addr as *mut U);

            // This can't use `ptr = ptr.offset(1)` because if U is smaller
            // than a pointer, allocations are padded to pointer size.
            // `.offset(1)` doesn't know about the padding and therefore
            // wouldn't advance to the next allocation.
            addr += Self::allocation_size();
        }
    }

    /// Return the page containing the object `ptr` points to.
    pub fn find(ptr: Pointer<U>) -> *mut TypedPage<U> {
        let ptr: usize = ptr.into();
        let page_addr = ptr & !(PAGE_ALIGN - 1);
        page_addr as *mut TypedPage<U>
    }

    unsafe fn add_to_free_list(&mut self, p: *mut U) {
        let listp = p as *mut *mut ();
        *listp = self.header.freelist;
        assert_eq!(*listp, self.header.freelist);
        self.header.freelist = p as *mut ();
    }

    unsafe fn allocation_index(&self, ptr: Pointer<U>) -> usize {
        let base = self.first_object_addr();

        // Check that ptr is in range.
        assert!(ptr.as_void() >= base as _);
        assert!(ptr.as_void() < (base + (Self::capacity() * Self::allocation_size())) as _);

        let index = (ptr.as_usize() - base as usize) / Self::allocation_size();
        assert_eq!(
            (base + index * Self::allocation_size()) as *const (),
            ptr.as_void()
        );
        index
    }

    pub unsafe fn get_mark_bit(&mut self, ptr: Pointer<U>) -> bool {
        let index = self.allocation_index(ptr);
        self.header.mark_bits[index]
    }

    pub unsafe fn set_mark_bit(&mut self, ptr: Pointer<U>) {
        let index = self.allocation_index(ptr);
        self.header.mark_bits.set(index, true);
    }

    pub unsafe fn try_alloc(&mut self) -> Option<Pointer<U>> {
        let p = self.header.freelist;
        if p.is_null() {
            None
        } else {
            let listp = p as *mut *mut ();
            self.header.freelist = *listp;
            let ap = Pointer::new(p as *mut U);
            let index = self.allocation_index(ap);
            assert!(!self.header.allocated_bits[index]);
            self.header.allocated_bits.set(index, true);
            Some(ap)
        }
    }

    unsafe fn sweep(&mut self) {
        let mut addr = self.first_object_addr();
        for i in 0..Self::capacity() {
            if self.header.allocated_bits[i] && !self.header.mark_bits[i] {
                ptr::drop_in_place(addr as *mut U);
                self.header.allocated_bits.set(i, false);
                self.add_to_free_list(addr as *mut U);
            }
            addr += Self::allocation_size();
        }
    }
}

unsafe fn sweep_entry_point<'h, T: IntoHeap<'h>>(header: &mut PageHeader) {
    header.downcast_mut::<T>().unwrap().sweep();
}

pub struct PageBox(*mut PageHeader);

impl PageBox {
    pub fn new<'h, T: IntoHeap<'h>>(heap: *mut Heap) -> PageBox {
        assert!(
            mem::size_of::<TypedPage<T::In>>() <= TypedPage::<T::In>::first_allocation_offset()
        );
        assert!(
            TypedPage::<T::In>::first_allocation_offset() +
                TypedPage::<T::In>::capacity() * TypedPage::<T::In>::allocation_size() <=
                PAGE_SIZE
        );
        let raw_page: *mut () = {
            let mut vec: Vec<u8> = Vec::with_capacity(PAGE_SIZE);
            let page = vec.as_mut_ptr() as *mut ();

            // Rust makes no guarantee whatsoever that this will work.
            // If it doesn't, panic.
            assert!(is_aligned(page));

            mem::forget(vec);
            page
        };

        let page: *mut TypedPage<T::In> = raw_page as *mut TypedPage<T::In>;
        let capacity = TypedPage::<T::In>::capacity();
        unsafe {
            ptr::write(
                page,
                TypedPage {
                    header: PageHeader {
                        heap: heap,
                        mark_bits: BitVec::from_elem(capacity, false),
                        allocated_bits: BitVec::from_elem(capacity, false),
                        mark_fn: mark_entry_point::<T>,
                        sweep_fn: sweep_entry_point::<T>,
                        freelist: ptr::null_mut(),
                    },
                    allocations: PhantomData,
                },
            );
            TypedPage::init(&mut *page);
            PageBox(&mut (*page).header as *mut PageHeader)
        }
    }
}

impl ::std::ops::Deref for PageBox {
    type Target = PageHeader;
    fn deref(&self) -> &PageHeader {
        unsafe { &*self.0 }
    }
}

impl ::std::ops::DerefMut for PageBox {
    fn deref_mut(&mut self) -> &mut PageHeader {
        unsafe { &mut *self.0 }
    }
}

impl Drop for PageBox {
    fn drop(&mut self) {
        unsafe {
            assert!((*self.0).is_empty());

            ptr::drop_in_place(self.0);

            // Deallocate the memory.
            Vec::from_raw_parts(self.0 as *mut u8, 0, PAGE_SIZE);
        }
    }
}
