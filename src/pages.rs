///! Allocating pages of memory from the OS and carving them into individual allocations.

use std::{mem, ptr};
use bit_vec::BitVec;
use traits::{mark_entry_point, GCThing, gcthing_type_id};
use heap::Heap;

/// Total number of objects that can be allocated in a single page at once.
///
/// XXX BOGUS: At present, this is number is carefully selected (with insider
/// knowledge of both `size_of::<PairStorage>()` on my platform and the `Vec`
/// allocator's behavior) so that a TypedPage object fits in a page and thus
/// the assertions below about PAGE_ALIGN pass.
pub const HEAP_SIZE: usize = 124;

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
    mark_fn: unsafe fn(*mut ()),
    sweep_fn: unsafe fn(*mut PageHeader<'a>),
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

    pub unsafe fn mark(&self, ptr: *mut ()) {
        (self.mark_fn)(ptr);
    }

    pub unsafe fn sweep(&mut self) {
        (self.sweep_fn)(self);
    }

    pub fn type_id(&self) -> usize {
        self.mark_fn as usize
    }
}

pub struct TypedPage<'a, T: GCThing<'a>> {
    pub header: PageHeader<'a>,
    objects: [T; HEAP_SIZE]
}

impl<'a, T: GCThing<'a>> TypedPage<'a, T> {
    unsafe fn init(&mut self) {
        for i in 0 .. HEAP_SIZE {
            let p = &mut self.objects[i] as *mut T;
            self.add_to_free_list(p);
        }
    }

    pub fn find(ptr: *const T) -> *mut TypedPage<'a, T> {
        let page_addr = ptr as usize & !(PAGE_ALIGN - 1);
        let page = page_addr as *mut TypedPage<'a, T>;
        assert_eq!(gcthing_type_id::<T>(), unsafe { (*page).header.type_id() });
        page
    }

    unsafe fn add_to_free_list(&mut self, p: *mut T) {
        let listp = p as *mut *mut ();
        *listp = self.header.freelist;
        assert_eq!(*listp, self.header.freelist);
        self.header.freelist = p as *mut ();
    }

    unsafe fn allocation_index(&self, ptr: *const T) -> usize {
        let base = &self.objects[0] as *const T;

        // Check that ptr is in range.
        assert!(ptr >= base);
        assert!(ptr < base.offset(HEAP_SIZE as isize));

        let index = (ptr as usize - base as usize) / mem::size_of::<T>();
        assert_eq!(&self.objects[index] as *const T, ptr);
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
        for i in 0 .. HEAP_SIZE {
            let p = &mut self.objects[i] as *mut T;
            if self.header.allocated_bits[i] && !self.header.mark_bits[i] {
                // The next statement has the effect of calling p's destructor.
                ptr::read(p);
                self.header.allocated_bits.set(i, false);
                self.add_to_free_list(p);
            }
        }
    }
}

unsafe fn sweep_entry_point<'a, T: GCThing<'a>>(header: *mut PageHeader<'a>) {
    // little reinterpret_cast as downcast, no worries
    let typed_page = header as *mut TypedPage<'a, T>;
    (*typed_page).sweep();
}

pub struct PageBox<'a>(*mut PageHeader<'a>);

impl<'a> PageBox<'a> {
    pub fn new<T: GCThing<'a>>(heap: &mut Heap<'a>) -> PageBox<'a> {
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
        unsafe {
            ptr::write(page, TypedPage {
                header: PageHeader {
                    heap: heap as *mut Heap<'a>,
                    mark_bits: BitVec::from_elem(HEAP_SIZE, false),
                    allocated_bits: BitVec::from_elem(HEAP_SIZE, false),
                    mark_fn: mark_entry_point::<T>,
                    sweep_fn: sweep_entry_point::<T>,
                    freelist: ptr::null_mut()
                },
                objects: mem::uninitialized()
            });
            TypedPage::<'a, T>::init(&mut *page);
            PageBox(&mut (*page).header as *mut PageHeader<'a>)
        }
    }

    pub fn downcast_mut<T: GCThing<'a>>(&mut self) -> Option<&mut TypedPage<'a, T>> {
        if gcthing_type_id::<T>() == unsafe { (*self.0).type_id() } {
            let ptr = self.0 as *mut TypedPage<'a, T>;
            Some(unsafe { &mut *ptr })
        } else {
            None
        }
    }

    pub fn header(&self) -> &PageHeader<'a> {
        unsafe { &*self.0 }
    }

    pub fn header_mut(&mut self) -> &mut PageHeader<'a> {
        unsafe { &mut *self.0 }
    }

    pub fn sweep(&mut self) {
        unsafe { (*self.0).sweep(); }
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

impl<'a> ::std::hash::Hash for PageBox<'a> {
    fn hash<H: ::std::hash::Hasher>(&self, hasher: &mut H) {
        hasher.write_usize(self.header().type_id());
    }
}
