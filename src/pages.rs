///! Allocating pages of memory from the OS and carving them into individual allocations.

use std::{mem, ptr};
use bit_vec::BitVec;
use heap::{GCThing, Heap, mark_entry_point};

/// Total number of Pair objects that can be allocated in a Heap at once.
///
/// XXX BOGUS: At present, this is number is carefully selected (with insider
/// knowledge of both `size_of::<PairStorage>()` on my platform and the `Box`
/// allocator's behavior) so that a TypedPage object fits in a page and thus
/// the assertions below about PAGE_ALIGN pass.
pub const HEAP_SIZE: usize = 125;

/// We rely on all bits to the right of this bit being 0 in addresses of
/// TypedPage instances.
pub const PAGE_ALIGN: usize = 0x1000;

// The heap is (for now) just a big array of Pairs
pub struct TypedPage<'a, T: GCThing<'a>> {
    pub heap: *mut Heap<'a>,
    pub mark_bits: BitVec,
    pub allocated_bits: BitVec,
    pub mark_entry_point: unsafe fn(*mut ()),
    freelist: *mut (),
    objects: [T; HEAP_SIZE]
}

impl<'a, T: GCThing<'a>> Drop for TypedPage<'a, T> {
    fn drop(&mut self) {
        // All the memory in self.objects is uninitialized at this point. Rust
        // will drop each of those objects, which would crash. So we have this
        // super lame hack: initialize all the objects with innocuous values so
        // they can be safely destroyed.
        for i in 0 .. HEAP_SIZE {
            unsafe {
                ptr::write(
                    &mut self.objects[i] as *mut T,
                    T::default());
            }
        }
    }
}

impl<'a, T: GCThing<'a>> TypedPage<'a, T> {
    pub unsafe fn new(heap: *mut Heap<'a>) -> Box<TypedPage<'a, T>> {
        let mut typed_page = Box::new(TypedPage {
            heap: heap,
            mark_bits: BitVec::from_elem(HEAP_SIZE, false),
            allocated_bits: BitVec::from_elem(HEAP_SIZE, false),
            mark_entry_point: mark_entry_point::<T>,
            freelist: ptr::null_mut(),
            objects: mem::uninitialized()
        });

        // These assertions will likely fail on 32-bit platforms or if someone
        // is somehow using a custom Box allocator. If either one fails, this
        // GC will not work.
        assert_eq!(&mut *typed_page as *mut TypedPage<'a, T> as usize & (PAGE_ALIGN - 1), 0);
        assert!(mem::size_of::<TypedPage<'a, T>>() <= PAGE_ALIGN);

        for i in 0 .. HEAP_SIZE {
            let p = &mut typed_page.objects[i] as *mut T;
            typed_page.add_to_free_list(p);
        }
        typed_page
    }

    unsafe fn add_to_free_list(&mut self, p: *mut T) {
        let listp = p as *mut *mut ();
        *listp = self.freelist;
        assert_eq!(*listp, self.freelist);
        self.freelist = p as *mut ();
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
        self.mark_bits[index]
    }

    pub unsafe fn set_mark_bit(&mut self, ptr: *const T) {
        let index = self.allocation_index(ptr);
        self.mark_bits.set(index, true);
    }

    pub unsafe fn try_alloc(&mut self) -> Option<*mut T> {
        let p = self.freelist;
        if p.is_null() {
            None
        } else {
            let listp = p as *mut *mut ();
            self.freelist = *listp;
            let ap = p as *mut T;
            let index = self.allocation_index(ap);
            assert!(!self.allocated_bits[index]);
            self.allocated_bits.set(index, true);
            Some(ap)
        }
    }

    pub unsafe fn sweep(&mut self) {
        for i in 0 .. HEAP_SIZE {
            let p = &mut self.objects[i] as *mut T;
            if self.allocated_bits[i] && !self.mark_bits[i] {
                // The next statement has the effect of calling p's destructor.
                ptr::read(p);
                self.allocated_bits.set(i, false);
                self.add_to_free_list(p);
            }
        }
    }
}
