//! Allocating pages of memory from the OS and carving them into individual
//! allocations.

use bit_vec::BitVec;
use heap::{GcHeap, HeapSessionId};
use marking::MarkingTracer;
use ptr::{Pointer, UntypedPointer};
use std::{cmp, mem, ptr};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use traits::{IntoHeapAllocation, Tracer};

/// Non-inlined function that serves as an entry point to marking. This is used
/// for marking root set entries.
unsafe fn mark_entry_point<'h, T>(addr: UntypedPointer, tracer: &mut MarkingTracer)
where
    T: IntoHeapAllocation<'h>,
{
    let addr = addr.as_typed_ptr::<T::In>();

    if GcHeap::get_mark_bit::<T>(addr) {
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

/// A unique id for each type that implements `IntoHeapAllocation`.
///
/// Implementation note: function types don't support Eq, so we cast to a
/// meaningless pointer type.
#[derive(Debug, Hash, PartialEq, Eq)]
pub struct TypeId(*const ());

pub fn heap_type_id<'h, T: IntoHeapAllocation<'h>>() -> TypeId {
    TypeId(mark_entry_point::<T> as *const ())
}

pub(crate) const PAGE_SIZE: usize = 0x1000;

/// We rely on all bits to the right of this bit being 0 in addresses of
/// TypedPage instances.
pub(crate) const PAGE_ALIGN: usize = 0x1000;

fn is_aligned(ptr: *const ()) -> bool {
    ptr as usize & (PAGE_ALIGN - 1) == 0
}

pub struct PageHeader {
    pub heap: *mut GcHeap,
    next_page: *mut PageHeader,
    mark_bits: BitVec,
    allocated_bits: BitVec,
    mark_fn: unsafe fn(UntypedPointer, &mut MarkingTracer),
    freelist: *mut (),
}

impl PageHeader {
    pub fn find(ptr: UntypedPointer) -> *mut PageHeader {
        let header_addr = ptr.as_usize() & !(PAGE_ALIGN - 1);
        assert!(header_addr != 0);
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

    pub fn type_id(&self) -> TypeId {
        TypeId(self.mark_fn as *const ())
    }

    pub fn downcast_mut<'h, T>(&mut self) -> Option<&mut TypedPage<T::In>>
    where
        T: IntoHeapAllocation<'h>,
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

impl<U> Deref for TypedPage<U> {
    type Target = PageHeader;

    fn deref(&self) -> &PageHeader {
        &self.header
    }
}

impl<U> DerefMut for TypedPage<U> {
    fn deref_mut(&mut self) -> &mut PageHeader {
        &mut self.header
    }
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

    unsafe fn init_freelist(&mut self) {
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
        PageHeader::find(ptr.into()) as *mut TypedPage<U>
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

    pub unsafe fn get_mark_bit(&self, ptr: Pointer<U>) -> bool {
        let index = self.allocation_index(ptr);
        self.header.mark_bits[index]
    }

    pub unsafe fn set_mark_bit(&mut self, ptr: Pointer<U>) {
        let index = self.allocation_index(ptr);
        assert!(self.header.allocated_bits.get(index).unwrap(),
                "marking memory that isn't allocated (dangling pointer?)");
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

    unsafe fn sweep(&mut self) -> bool {
        let mut addr = self.first_object_addr();
        let mut swept_any = false;
        for i in 0..Self::capacity() {
            if self.header.allocated_bits[i] && !self.header.mark_bits[i] {
                ptr::drop_in_place(addr as *mut U);
                if cfg!(debug_assertions) || cfg!(test) {
                    // Paint the unused memory with a known-bad value.
                    const SWEPT_BYTE: u8 = 0xf4;
                    ptr::write_bytes(addr as *mut U, SWEPT_BYTE, 1);
                }
                self.header.allocated_bits.set(i, false);
                self.add_to_free_list(addr as *mut U);
                swept_any = true;
            }
            addr += Self::allocation_size();
        }
        swept_any
    }
}

/// Sweep a page.
///
/// # Safety
///
/// This must be called only after a full mark phase, to avoid sweeping objects
/// that are still reachable.
unsafe fn sweep_entry_point<'h, T: IntoHeapAllocation<'h>>(header: &mut PageHeader) -> bool {
    header.downcast_mut::<T>().expect("page header corrupted").sweep()
}

/// An unordered collection of memory pages that all share an allocation type.
///
/// All pages in this collection have matching `.heap` and `.mark_fn` fields.
pub struct PageSet {
    heap: *mut GcHeap,

    sweep_fn: unsafe fn(&mut PageHeader) -> bool,

    /// Total number of pages in the following lists.
    page_count: usize,

    /// Head of the linked list of fully allocated pages.
    full_pages: *mut PageHeader,

    /// Head of the linkedlist of nonfull pages.
    other_pages: *mut PageHeader,

    /// The maximum number of pages, or None for no limit.
    limit: Option<usize>,
}

/// Apply a closure to every page in a linked list.
fn each_page<F: FnMut(&PageHeader)>(first_page: *mut PageHeader, mut f: F) {
    unsafe {
        let mut page = first_page;
        while !page.is_null() {
            let header = &*page;
            f(header);
            page = header.next_page;
        }
    }
}

/// Apply a closure to every page in a linked list.
fn each_page_mut<F: FnMut(&mut PageHeader)>(first_page: *mut PageHeader, mut f: F) {
    unsafe {
        let mut page = first_page;
        while !page.is_null() {
            let header = &mut *page;
            f(header);
            page = header.next_page;
        }
    }
}

impl Drop for PageSet {
    fn drop(&mut self) {
        assert!(self.full_pages.is_null());
        unsafe {
            // Don't use each_page here: we're dropping them.
            let mut p = self.other_pages;
            while !p.is_null() {
                let next = (*p).next_page;
                ptr::drop_in_place(p); // drop the header
                Vec::from_raw_parts(p as *mut u8, 0, PAGE_SIZE); // free the page
                p = next;
            }
        }
    }
}

impl PageSet {
    /// Create a new PageSet.
    ///
    /// # Safety
    ///
    /// Safe as long as `heap` is a valid pointer.
    pub unsafe fn new<'h, T: IntoHeapAllocation<'h>>(heap: *mut GcHeap) -> PageSet {
        PageSet {
            heap,
            sweep_fn: sweep_entry_point::<T>,
            page_count: 0,
            full_pages: ptr::null_mut(),
            other_pages: ptr::null_mut(),
            limit: None,
        }
    }

    /// Downcast to a typed PageSetRef.
    ///
    /// # Panics
    ///
    /// If T is not the actual allocation type for this page set.
    pub fn downcast_mut<'a, 'h, T>(&'a mut self) -> PageSetRef<'a, 'h, T>
    where
        T: IntoHeapAllocation<'h> + 'a,
    {
        assert_eq!(
            self.sweep_fn as *const (),
            sweep_entry_point::<T> as *const ()
        );

        PageSetRef {
            page_set: self,
            id: PhantomData,
            also: PhantomData,
        }
    }

    fn each_page<F: FnMut(&PageHeader)>(&self, mut f: F) {
        each_page(self.full_pages, &mut f);
        each_page(self.other_pages, &mut f);
    }

    fn each_page_mut<F: FnMut(&mut PageHeader)>(&mut self, mut f: F) {
        each_page_mut(self.full_pages, &mut f);
        each_page_mut(self.other_pages, &mut f);
    }

    /// Clear mark bits from each page in this set.
    ///
    /// # Safety
    ///
    /// This must be called only at the beginning of a GC cycle.
    pub unsafe fn clear_mark_bits(&mut self) {
        self.each_page_mut(|page| page.clear_mark_bits());
    }

    /// Sweep all unmarked objects from all pages.
    ///
    /// # Safety
    ///
    /// Safe to call only as the final part of GC.
    pub unsafe fn sweep(&mut self) {
        // Sweep nonfull pages.
        each_page_mut(self.other_pages, |page| {
            (self.sweep_fn)(page);
        });

        // Sweep full pages. Much more complicated because we have to move
        // pages from one list to the other if any space is freed.
        let mut prev_page = &mut self.full_pages;
        let mut page = *prev_page;
        while !page.is_null() {
            if (self.sweep_fn)(&mut *page) {
                let next_page = (*page).next_page;

                // remove from full list
                *prev_page = next_page;

                // add to nonfull list
                (*page).next_page = self.other_pages;
                self.other_pages = page;

                page = next_page;
            } else {
                prev_page = &mut (*page).next_page;
                page = *prev_page;
            }
        }
    }

    /// True if nothing is allocated in this set of pages.
    pub fn all_pages_are_empty(&self) -> bool {
        let mut empty = true;
        self.each_page(|page| { empty &= page.is_empty(); });
        empty
    }

    pub fn set_page_limit(&mut self, limit: Option<usize>) {
        self.limit = limit;
    }
}

pub struct PageSetRef<'a, 'h, T: IntoHeapAllocation<'h> + 'a> {
    page_set: &'a mut PageSet,
    id: HeapSessionId<'h>,
    also: PhantomData<&'a mut T>,
}

impl<'a, 'h, T: IntoHeapAllocation<'h> + 'a> Deref for PageSetRef<'a, 'h, T> {
    type Target = PageSet;

    fn deref(&self) -> &PageSet { self.page_set }
}

impl<'a, 'h, T: IntoHeapAllocation<'h> + 'a> DerefMut for PageSetRef<'a, 'h, T> {
    fn deref_mut(&mut self) -> &mut PageSet { self.page_set }
}

impl<'a, 'h, T: IntoHeapAllocation<'h> + 'a> PageSetRef<'a, 'h, T> {
    /// Allocate memory for a value of type `T::In`.
    ///
    /// # Safety
    ///
    /// Safe to call as long as GC is not happening.
    pub unsafe fn try_alloc(&mut self) -> Option<Pointer<T::In>> {
        // First, try to allocate from an existing page.
        let front_page = self.other_pages;
        if !front_page.is_null() {
            // We have a nonfull page. Allocation can't fail.
            assert!(!(*front_page).freelist.is_null());
            let page = (*front_page).downcast_mut::<T>().unwrap();
            let ptr = page.try_alloc().unwrap();

            // If the page is full now, move it to the other list.
            if page.freelist.is_null() {
                // Pop this page from the nonfull page list.
                self.other_pages = page.next_page;

                // Add it to the full-page list.
                page.next_page = self.full_pages;
                self.full_pages = &mut page.header;
            }
            return Some(ptr);
        }

        // If there is a limit and we already have at least that many pages, fail.
        match self.limit {
            Some(limit) if self.page_count >= limit => None,
            _ => self.new_page().try_alloc(),
        }
    }

    /// Allocate a page from the operating system.
    ///
    /// Initialize its header and freelist and link it into this page set's
    /// linked list of pages.
    fn new_page(&mut self) -> &mut TypedPage<T::In> {
        assert!({
            let size_of_page = mem::size_of::<TypedPage<T::In>>();
            let alloc_offset = TypedPage::<T::In>::first_allocation_offset();
            size_of_page <= alloc_offset
        });
        assert!({
            let alloc_offset = TypedPage::<T::In>::first_allocation_offset();
            let alloc_size = TypedPage::<T::In>::allocation_size();
            let capacity = TypedPage::<T::In>::capacity();
            alloc_offset + capacity * alloc_size <= PAGE_SIZE
        });

        let mut vec: Vec<u8> = Vec::with_capacity(PAGE_SIZE);
        let raw_page = vec.as_mut_ptr() as *mut ();

        // Rust makes no guarantee whatsoever that this will work.
        // If it doesn't, panic.
        assert!(is_aligned(raw_page));

        let page_ptr: *mut TypedPage<T::In> = raw_page as *mut TypedPage<T::In>;
        let capacity = TypedPage::<T::In>::capacity();
        unsafe {
            // Normally we insert the new page in the nonfull page list.
            // However, if T::In is so large that only one allocation fits in a
            // page, the new page must go directly into the full page list.
            let list_head =
                if capacity == 1 {
                    &mut self.page_set.full_pages
                } else {
                    &mut self.page_set.other_pages
                };

            ptr::write(
                page_ptr,
                TypedPage {
                    header: PageHeader {
                        heap: self.page_set.heap,
                        next_page: *list_head,
                        mark_bits: BitVec::from_elem(capacity, false),
                        allocated_bits: BitVec::from_elem(capacity, false),
                        mark_fn: mark_entry_point::<T>,
                        freelist: ptr::null_mut(),
                    },
                    allocations: PhantomData,
                },
            );

            let page = &mut *page_ptr;
            page.init_freelist();

            // Remove the memory from the vector and link it into
            // the PageSet's linked list.
            mem::forget(vec);
            *list_head = &mut page.header;
            self.page_set.page_count += 1;

            page
        }
    }
}
