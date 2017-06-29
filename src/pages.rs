//! Allocating pages of memory from the OS and carving them into individual
//! allocations.

use std::{cmp, mem, ptr};
use std::marker::PhantomData;
use bit_vec::BitVec;
use traits::{IntoHeapAllocation, Tracer};
use heap::{Heap, HeapSessionId};
use marking::MarkingTracer;
use ptr::{Pointer, UntypedPointer};

/// Non-inlined function that serves as an entry point to marking. This is used
/// for marking root set entries.
unsafe fn mark_entry_point<'h, T>(addr: UntypedPointer, tracer: &mut MarkingTracer)
where
    T: IntoHeapAllocation<'h>
{
    let addr = addr.as_typed_ptr::<T::In>();

    if Heap::get_mark_bit::<T>(addr) {
        // If the mark bit is set, then this object is gray in the classic
        // tri-color sense: seen but we just popped it off the mark stack and
        // have't finished enumerating its outgoing edges.
        T::trace(addr.as_ref(), tracer);
    } else {
        // If the mark bit is not set, then this object is white in the classic
        // tri-color sense: freshly discovered to be live, and we now need to
        // set its mark bit and then process its edges or push it onto the mark
        // stack for later edge processing.
        tracer.visit::<T>(addr);
    }
}

pub fn heap_type_id<'h, T: IntoHeapAllocation<'h>>() -> usize {
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
    mark_fn: unsafe fn(UntypedPointer, &mut MarkingTracer),
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

    pub unsafe fn mark(&self, ptr: UntypedPointer, tracer: &mut MarkingTracer) {
        (self.mark_fn)(ptr, tracer);
    }

    pub fn type_id(&self) -> usize {
        self.mark_fn as usize
    }

    pub fn downcast_mut<'h, T>(&mut self) -> Option<&mut TypedPage<T::In>>
    where
        T: IntoHeapAllocation<'h>
    {
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
    if a == n {
        n
    } else {
        n + k
    }
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

    /// Allocate a `U`-sized-and-aligned region of uninitialized memory
    /// from this page.
    ///
    /// # Safety
    ///
    /// This is safe unless GC is happening.
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

/// Sweep a page.
///
/// # Safety
///
/// This must be called only after a full mark phase, to avoid sweeping objects
/// that are still reachable.
unsafe fn sweep_entry_point<'h, T: IntoHeapAllocation<'h>>(header: &mut PageHeader) {
    header.downcast_mut::<T>().unwrap().sweep();
}

pub struct PageBox(*mut PageHeader);

impl PageBox {
    fn new<'h, T: IntoHeapAllocation<'h>>(heap: *mut Heap) -> PageBox {
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

/// An unordered collection of memory pages that all share an allocation type.
pub struct PageSet {
    heap: *mut Heap,

    sweep_fn: unsafe fn(&mut PageHeader),

    /// The collection of pages.
    ///
    /// All pages in this collection have matching `.heap`, `.mark_fn`, and
    /// `.sweep_fn` fields.
    pages: Vec<PageBox>,

    /// The maximum number of pages, or None for no limit.
    limit: Option<usize>,
}

impl PageSet {
    /// Create a new PageSet.
    ///
    /// # Safety
    ///
    /// Safe as long as `heap` is a valid pointer.
    pub unsafe fn new<'h, T: IntoHeapAllocation<'h>>(heap: *mut Heap) -> PageSet {
        PageSet {
            heap,
            sweep_fn: sweep_entry_point::<T>,
            pages: vec![],
            limit: None
        }
    }

    /// Downcast to a typed PageSetRef.
    ///
    /// # Panics
    ///
    /// If T is not the actual allocation type for this page set.
    pub fn downcast_mut<'a, 'h, T>(&'a mut self) -> PageSetRef<'a, 'h, T>
    where
        T: IntoHeapAllocation<'h> + 'a
    {
        assert_eq!(self.sweep_fn as *const (), sweep_entry_point::<T> as *const ());

        PageSetRef {
            pages: self,
            id: PhantomData,
            also: PhantomData
        }
    }

    /// Clear mark bits from each page in this set.
    ///
    /// # Safety
    ///
    /// This must be called only at the beginning of a GC cycle.
    pub unsafe fn clear_mark_bits(&mut self) {
        for page in &mut self.pages {
            page.clear_mark_bits();
        }
    }

    /// Sweep all unmarked objects from all pages.
    ///
    /// # Safety
    ///
    /// Safe to call only as the final part of GC.
    pub unsafe fn sweep(&mut self) {
        for page in &mut self.pages {
            (self.sweep_fn)(page);
        }
    }

    /// Assert that nothing is allocated in this set of pages.
    pub fn assert_no_allocations(&self) {
        for page in &self.pages {
            assert!(page.is_empty());
        }
    }
}

pub struct PageSetRef<'a, 'h, T: IntoHeapAllocation<'h> + 'a> {
    pages: &'a mut PageSet,
    id: HeapSessionId<'h>,
    also: PhantomData<&'a mut T>
}

impl<'a, 'h, T: IntoHeapAllocation<'h> + 'a> PageSetRef<'a, 'h, T> {
    pub fn set_page_limit(&mut self, limit: Option<usize>) {
        self.pages.limit = limit;
    }

    /// Allocate memory for a value of type `T::In`.
    ///
    /// # Safety
    ///
    /// Safe to call as long as GC is not happening.
    pub unsafe fn try_alloc(&mut self) -> Option<Pointer<T::In>> {
        // First, try to allocate from each existing page. (Obviously slow.)
        for page in &mut self.pages.pages {
            let page = page.downcast_mut::<T>().unwrap();
            if let Some(ptr) = page.try_alloc() {
                return Some(ptr);
            }
        }

        // If there is a limit and we already have at least that many pages, fail.
        match self.pages.limit {
            Some(limit) if self.pages.pages.len() >= limit => None,
            _ => {
                let mut page = PageBox::new::<T>(self.pages.heap);
                let alloc = page.downcast_mut::<T>().unwrap().try_alloc();
                self.pages.pages.push(page);
                alloc
            }
        }
    }
}
