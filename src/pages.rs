//! Allocating pages of memory from the OS and carving them into individual
//! allocations. See TypedPage for details.

use heap::GcHeap;
use marking::MarkingTracer;
use ptr::{Pointer, UntypedPointer};
use std::any::TypeId;
use std::{cmp, mem, ptr};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use poison;
use traits::{InHeap, Tracer};

/// Stores mark bits, pin counts, and an "am I in use?" bit for heap allocations.
struct MarkWord(usize);

const MARK_BIT: usize = 1;
const ALLOCATED_BIT: usize = 2;

/// Add the value `*p` to the root set, protecting it from GC.
///
/// A value that has been pinned *n* times stays in the root set
/// until it has been unpinned *n* times.
///
/// # Safety
///
/// `p` must point to a live allocation of type `U` in this heap.
pub unsafe fn pin<U: InHeap>(p: Pointer<U>) {
    MarkWord::from_ptr(p, |mw| mw.pin());
}

/// Unpin a heap-allocated value (see `pin`).
///
/// # Safety
///
/// `p` must point to a pinned allocation of type `U` in this heap.
pub unsafe fn unpin<U: InHeap>(p: Pointer<U>) {
    MarkWord::from_ptr(p, |mw| mw.unpin());
}

/// Unpin a heap allocation.
///
/// # Safety
///
/// `p` must point to a pinned allocation in this heap.
pub unsafe fn unpin_untyped(p: UntypedPointer) {
    MarkWord::from_untyped_ptr(p, |mw| mw.unpin());
}

pub unsafe fn get_mark_bit<U: InHeap>(p: Pointer<U>) -> bool {
    MarkWord::from_ptr(p, |mw| mw.is_marked())
}

pub unsafe fn set_mark_bit<U: InHeap>(p: Pointer<U>) {
    MarkWord::from_ptr(p, |mw| mw.mark());
}

const MARK_WORD_INIT: MarkWord = MarkWord(0);

impl MarkWord {
    unsafe fn from_ptr<U: InHeap, F, R>(ptr: Pointer<U>, f: F) -> R
        where F: for<'a> FnOnce(&'a mut MarkWord) -> R
    {
        let addr = ptr.as_usize() - mem::size_of::<MarkWord>();
        f(&mut *(addr as *mut MarkWord))
    }

    unsafe fn from_untyped_ptr<F, R>(ptr: UntypedPointer, f: F) -> R
        where F: for<'a> FnOnce(&'a mut MarkWord) -> R
    {
        let addr = ptr.as_usize() - mem::size_of::<MarkWord>();
        f(&mut *(addr as *mut MarkWord))
    }

    fn is_allocated(&self) -> bool {
        self.0 & ALLOCATED_BIT != 0
    }

    fn set_allocated(&mut self) {
        self.0 |= ALLOCATED_BIT;
    }

    fn clear_allocated(&mut self) {
        self.0 &= !ALLOCATED_BIT;
    }

    fn is_marked(&self) -> bool {
        self.0 & MARK_BIT != 0
    }

    fn mark(&mut self) {
        self.0 |= MARK_BIT;
    }

    fn unmark(&mut self) {
        self.0 &= !MARK_BIT;
    }

    fn is_pinned(&self) -> bool {
        self.0 >> 2 != 0
    }

    #[inline]
    fn pin(&mut self) {
        debug_assert!(self.is_allocated());
        self.0 += 4;
    }

    #[inline]
    fn unpin(&mut self) {
        debug_assert!(self.is_allocated());
        debug_assert!(self.is_pinned());
        self.0 -= 4;
    }
}

/// Non-inlined function that serves as an entry point to marking. This is used
/// for marking root set entries.
unsafe fn mark_entry_point<U: InHeap>(addr: UntypedPointer, tracer: &mut MarkingTracer) {
    let addr = addr.as_typed_ptr::<U>();

    if get_mark_bit(addr) {
        // If the mark bit is set, then this object is gray in the classic
        // tri-color sense: seen but we just popped it off the mark stack and
        // have't finished enumerating its outgoing edges.
        addr.as_ref().trace(tracer);
    } else {
        // If the mark bit is not set, then this object is white in the classic
        // tri-color sense: freshly discovered to be live, and we now need to
        // set its mark bit and then process its edges or push it onto the mark
        // stack for later edge processing.
        tracer.visit::<U>(addr);
    }
}

pub fn heap_type_id<U: InHeap>() -> TypeId {
    TypeId::of::<U>()
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
    type_id: TypeId,
    mark_fn: unsafe fn(UntypedPointer, &mut MarkingTracer),
    freelist: *mut (),
}

impl PageHeader {
    pub fn find(ptr: UntypedPointer) -> *mut PageHeader {
        let header_addr = ptr.as_usize() & !(PAGE_ALIGN - 1);
        debug_assert!(header_addr != 0);
        header_addr as *mut PageHeader
    }

    pub unsafe fn mark(&self, ptr: UntypedPointer, tracer: &mut MarkingTracer) {
        (self.mark_fn)(ptr, tracer);
    }

    pub fn type_id(&self) -> TypeId {
        self.type_id
    }

    pub fn downcast_mut<U: InHeap>(&mut self) -> Option<&mut TypedPage<U>> {
        if heap_type_id::<U>() == self.type_id() {
            let ptr = self as *mut PageHeader as *mut TypedPage<U>;
            Some(unsafe { &mut *ptr })
        } else {
            None
        }
    }

    pub fn unchecked_downcast_mut<U: InHeap>(&mut self) -> &mut TypedPage<U> {
        debug_assert_eq!(heap_type_id::<U>(), self.type_id());
        let ptr = self as *mut PageHeader as *mut TypedPage<U>;
        unsafe { &mut *ptr }
    }

    fn begin_offset() -> usize {
        mem::size_of::<PageHeader>()
    }

    /// Address of the first allocation on this page.
    fn begin(&self) -> usize {
        (self as *const PageHeader as usize) + Self::begin_offset()
    }

    /// Get a pointer just after the last object this page has capacity for.
    ///
    /// # Safety
    ///
    /// If the provided allocation size is incorrect, then the resulting pointer
    /// can be incorrect.
    unsafe fn end(&self, allocation_size: usize) -> usize {
        let capacity = (PAGE_SIZE - Self::begin_offset()) / allocation_size;
        self.begin() + capacity * allocation_size
    }

    /// Clear this page's mark bits.
    ///
    /// # Safety
    ///
    /// If the provided allocation size is incorrect, then this method could
    /// read and write the incorrect memory locations.
    pub unsafe fn clear_mark_bits(&mut self, allocation_size: usize, roots: &mut Vec<UntypedPointer>) {
        let mut addr = self.begin();
        let end = self.end(allocation_size);
        while addr < end {
            let mark_word = &mut *(addr as *mut MarkWord);
            if mark_word.is_pinned() {
                mark_word.mark();
                let ptr = UntypedPointer::new((addr + mem::size_of::<MarkWord>()) as *const ());
                roots.push(ptr);
            } else {
                mark_word.unmark();
            }
            addr += allocation_size;
        }
    }

    /// Collect all the roots in this page.
    pub unsafe fn mark_and_collect_roots(&mut self, allocation_size: usize, roots: &mut Vec<UntypedPointer>) {
        let mut addr = self.begin();
        let end = self.end(allocation_size);
        while addr < end {
            let mark_word = &mut *(addr as *mut MarkWord);
            if mark_word.is_pinned() {
                mark_word.mark();
                let ptr = UntypedPointer::new((addr + mem::size_of::<MarkWord>()) as *const ());
                roots.push(ptr);
            }
            addr += allocation_size;
        }
    }

    /// True if nothing on this page is allocated.
    ///
    /// # Safety
    ///
    /// If the provided allocation size is incorrect, then this method could
    /// read incorrect memory locations.
    pub unsafe fn is_empty(&self, allocation_size: usize) -> bool {
        let mut addr = self.begin();
        let end = self.end(allocation_size);
        while addr < end {
            let mark_word = &mut *(addr as *mut MarkWord);
            if mark_word.is_allocated() {
                return false;
            }
            addr += allocation_size;
        }
        true
    }
}

/// A page of memory where heap-allocated objects of a particular type are stored.
///
/// A GcHeap is a collection of PageSets, and each PageSet is a collection of
/// TypedPages.
///
/// The layout of a page is like this:
///
/// ```ignore
/// struct TypedPage<U: InHeap> {
///     header: PageHeader
///     allocations: [Allocation<U>; Self::capacity()]
/// }
///
/// struct Allocation<U: InHeap> {
///     mark_word: MarkWord,
///     union {
///         value: U,
///         free_list_chain: *mut U,
///     }
/// }
/// ```
///
/// where `Self::capacity()` is computed so as to make all this fit in 4KB.
/// (Allocations larger than 4KB are not supported.)
///
/// Since Rust doesn't support that particular kind of union yet, we implement
/// this data structure with pointer arithmetic and hackery.
///
/// The `(MarkWord, U)` pairs in the page are called "allocations".  Each
/// allocation is either in use (containing an actual value of type `U`) or
/// free (uninitialized memory). The `MarkWord` distinguishes these two cases.
/// All free allocations are in the freelist.
///
/// In addition, an allocation that's in use can be "pinned", making it part of
/// the root set. GcRefs outside the heap keep their referents pinned.
///
/// Trivia: This wastes a word when size_of<U>() is 0; the MarkWord (rather
/// than the value field) could contain the free-list chain. However, the
/// direction we'd like to go is to get rid of pin counts.
pub struct TypedPage<U: InHeap> {
    pub header: PageHeader,
    pub allocations: PhantomData<U>,
}

impl<U: InHeap> Deref for TypedPage<U> {
    type Target = PageHeader;

    fn deref(&self) -> &PageHeader {
        &self.header
    }
}

impl<U: InHeap> DerefMut for TypedPage<U> {
    fn deref_mut(&mut self) -> &mut PageHeader {
        &mut self.header
    }
}

/// Returns the smallest multiple of `k` that is at least `n`.
///
/// Panics if the answer is too big to fit in a `usize`.
#[inline(always)]
fn round_up(n: usize, k: usize) -> usize {
    let a = n / k * k;
    if a == n {
        n
    } else {
        n + k
    }
}

impl<U: InHeap> TypedPage<U> {
    /// The actual size of an allocation can't be smaller than the size of a
    /// pointer, due to the way we store the freelist by stealing a pointer
    /// from the allocation itself.
    fn allocation_size() -> usize {
        mem::size_of::<MarkWord>() + round_up(cmp::max(mem::size_of::<U>(), mem::size_of::<*mut U>()),
                                              mem::align_of::<MarkWord>())
    }

    /// Offset, in bytes, of the first allocation from the start of the page.
    pub(crate) fn first_allocation_offset() -> usize {
        mem::size_of::<PageHeader>()
    }

    /// Number of allocations that fit in a page.
    pub fn capacity() -> usize {
        (PAGE_SIZE - Self::first_allocation_offset()) / Self::allocation_size()
    }

    /// Address of the first allocation in this page.
    fn begin(&self) -> usize {
        (self as *const Self as usize) + Self::first_allocation_offset()
    }

    /// Address one past the end of this page's array of allocations.
    fn end(&self) -> usize {
        // Everything after the first plus sign here is a constant expression.
        //
        // Addition will overflow if `self` is literally the last page in
        // virtual memory—which can't happen—and the constant works out to
        // PAGE_SIZE, which can.
        (self as *const Self as usize) + (Self::first_allocation_offset() +
                                          Self::capacity() * Self::allocation_size())
    }

    unsafe fn init_mark_words_and_freelist(&mut self) {
        let mut addr = self.begin();
        let end = self.end();
        while addr < end {
            let mark_word = addr as *mut MarkWord;
            ptr::write(mark_word, MARK_WORD_INIT);
            self.add_to_free_list((addr + mem::size_of::<MarkWord>()) as *mut U);

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

    /// Allocate a `U`-sized-and-aligned region of uninitialized memory
    /// from this page.
    ///
    /// # Safety
    ///
    /// This is safe unless GC is happening.
    pub unsafe fn try_alloc(&mut self) -> Option<UninitializedAllocation<U>> {
        if self.header.freelist.is_null() {
            None
        } else {
            Some(self.infallible_alloc())
        }
    }

    /// Allocate a `U`-sized-and-aligned region of uninitialized memory
    /// from this page.
    ///
    /// # Safety
    ///
    /// This is safe if the freelist is not empty and GC is not happening.
    unsafe fn infallible_alloc(&mut self) -> UninitializedAllocation<U> {
        let listp = self.header.freelist as *mut *mut ();
        self.header.freelist = *listp;
        let ptr = Pointer::new(listp as *mut U);
        MarkWord::from_ptr(ptr, |mw| {
            debug_assert!(!mw.is_allocated());
            mw.set_allocated();
            // Clear the sticky bit, if it was stuck.
            mw.unmark();
        });
        UninitializedAllocation { ptr }
    }

    /// Sweep this page and return the number of objects swept.
    unsafe fn sweep(&mut self) -> usize {
        let mut num_swept = 0;

        let mut addr = self.begin();
        let end = self.end();
        while addr < end {
            let mw = &mut *(addr as *mut MarkWord);
            if mw.is_allocated() && !mw.is_marked() {
                let object_ptr = (addr + mem::size_of::<MarkWord>()) as *mut U;
                ptr::drop_in_place(object_ptr);
                poison::paint::swept(object_ptr);
                mw.clear_allocated();
                self.add_to_free_list(object_ptr);
                num_swept += 1;
            }
            addr += Self::allocation_size();
        }

        num_swept
    }
}

/// Sweep a page and return the number of objects swept.
///
/// # Safety
///
/// This must be called only after a full mark phase, to avoid sweeping objects
/// that are still reachable.
unsafe fn sweep_entry_point<U: InHeap>(header: &mut PageHeader) -> usize {
    header.downcast_mut::<U>().expect("page header corrupted").sweep()
}

/// An unordered collection of memory pages that all share an allocation type.
///
/// All pages in this collection have matching `.heap` and `.mark_fn` fields.
pub struct PageSet {
    heap: *mut GcHeap,
    allocation_size: usize,
    sweep_fn: unsafe fn(&mut PageHeader) -> usize,

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
        // Don't use each_page here: we're dropping them.
        for page_list in &[self.full_pages, self.other_pages] {
            let mut page = *page_list;
            while !page.is_null() {
                unsafe {
                    let mut roots_to_ignore = vec![];
                    let next = (*page).next_page;
                    (*page).clear_mark_bits(self.allocation_size, &mut roots_to_ignore);
                    (self.sweep_fn)(&mut *page); // drop all objects remaining in the page
                    ptr::drop_in_place(page); // drop the header
                    Vec::from_raw_parts(page as *mut u8, 0, PAGE_SIZE); // free the page
                    page = next;
                }
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
    pub unsafe fn new<'h, U: InHeap>(heap: *mut GcHeap) -> PageSet {
        PageSet {
            heap,
            allocation_size: TypedPage::<U>::allocation_size(),
            sweep_fn: sweep_entry_point::<U>,
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
    /// If U is not the actual allocation type for this page set.
    pub fn downcast_mut<'a, U: InHeap>(&'a mut self) -> PageSetRef<'a, U> {
        assert_eq!(
            self.sweep_fn as *const (),
            sweep_entry_point::<U> as *const ()
        );

        PageSetRef {
            page_set: self,
            phantom: PhantomData,
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
    pub unsafe fn clear_mark_bits(&mut self, roots: &mut Vec<UntypedPointer>) {
        let size = self.allocation_size;
        self.each_page_mut(|page| page.clear_mark_bits(size, roots));
    }

    /// Collect all the roots in this page set.
    pub fn mark_and_collect_roots(&mut self, roots: &mut Vec<UntypedPointer>) {
        let size = self.allocation_size;
        self.each_page_mut(|page| unsafe { page.mark_and_collect_roots(size, roots); });
    }

    /// Sweep all unmarked objects from all pages and return the number of
    /// objects swept.
    ///
    /// # Safety
    ///
    /// Safe to call only as the final part of GC.
    pub unsafe fn sweep(&mut self) -> usize {
        let mut num_swept = 0;

        // Sweep nonfull pages.
        each_page_mut(self.other_pages, |page| {
            num_swept += (self.sweep_fn)(page);
        });

        // Sweep full pages. Much more complicated because we have to move
        // pages from one list to the other if any space is freed.
        let mut prev_page = &mut self.full_pages;
        let mut page = *prev_page;
        while !page.is_null() {
            let num_swept_this_page = (self.sweep_fn)(&mut *page);
            num_swept += num_swept_this_page;
            if num_swept_this_page > 0 {
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

        num_swept
    }

    /// True if nothing is allocated in this set of pages.
    pub fn all_pages_are_empty(&self) -> bool {
        let mut empty = true;
        self.each_page(|page| {
            empty &= unsafe { page.is_empty(self.allocation_size) };
        });
        empty
    }

    pub fn set_page_limit(&mut self, limit: Option<usize>) {
        self.limit = limit;
    }
}

pub struct PageSetRef<'a, U: InHeap> {
    page_set: &'a mut PageSet,
    phantom: PhantomData<&'a mut U>,
}

impl<'a, U: InHeap> Deref for PageSetRef<'a, U> {
    type Target = PageSet;

    fn deref(&self) -> &PageSet { self.page_set }
}

impl<'a, U: InHeap> DerefMut for PageSetRef<'a, U> {
    fn deref_mut(&mut self) -> &mut PageSet { self.page_set }
}

impl<'a, U: InHeap> PageSetRef<'a, U> {
    pub unsafe fn try_fast_alloc(&mut self) -> Option<UninitializedAllocation<U>> {
        if self.other_pages.is_null() {
            None
        } else {
            Some(self.infallible_fast_alloc())
        }
    }

    unsafe fn infallible_fast_alloc(&mut self) -> UninitializedAllocation<U> {
        // We have a nonfull page. Allocation can't fail.
        debug_assert!(!self.other_pages.is_null());

        let front_page = self.other_pages;
        let page = (*front_page).unchecked_downcast_mut::<U>();
        let ptr = page.infallible_alloc();

        // If the page is full now, move it to the other list.
        if page.freelist.is_null() {
            // Pop this page from the nonfull page list.
            self.other_pages = page.next_page;

            // Add it to the full-page list.
            page.next_page = self.full_pages;
            self.full_pages = &mut page.header;
        }
        ptr
    }

    /// Allocate memory for a value of type `U`.
    ///
    /// # Safety
    ///
    /// Safe to call as long as GC is not happening.
    pub unsafe fn try_alloc(&mut self) -> Option<UninitializedAllocation<U>> {
        // First, try to allocate from an existing page.
        if !self.other_pages.is_null() {
            return Some(self.infallible_fast_alloc());
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
    fn new_page(&mut self) -> &mut TypedPage<U> {
        let capacity = TypedPage::<U>::capacity();
        assert!({
            let size_of_page = mem::size_of::<TypedPage<U>>();
            let alloc_offset = TypedPage::<U>::first_allocation_offset();
            size_of_page <= alloc_offset
        });
        assert!({
            let alloc_offset = TypedPage::<U>::first_allocation_offset();
            let alloc_size = TypedPage::<U>::allocation_size();
            alloc_offset + capacity * alloc_size <= PAGE_SIZE
        });

        // All allocations in a page are pointer-size-aligned. If this isn't
        // good enough for U, panic.
        {
            let word_size = mem::size_of::<usize>();
            assert_eq!(mem::size_of::<MarkWord>(), word_size);
            assert!(mem::align_of::<U>() <= word_size,
                    "Types with exotic alignment requirements are not supported");
        }

        let mut vec: Vec<u8> = Vec::with_capacity(PAGE_SIZE);
        let raw_page = vec.as_mut_ptr() as *mut ();

        // Rust makes no guarantee whatsoever that this will work.
        // If it doesn't, panic.
        assert!(is_aligned(raw_page));

        let page_ptr: *mut TypedPage<U> = raw_page as *mut TypedPage<U>;
        unsafe {
            // Normally we insert the new page in the nonfull page list.
            // However, if U is so large that only one allocation fits in a
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
                        type_id: heap_type_id::<U>(),
                        mark_fn: mark_entry_point::<U>,
                        freelist: ptr::null_mut(),
                    },
                    allocations: PhantomData,
                },
            );

            let page = &mut *page_ptr;
            page.init_mark_words_and_freelist();

            // Remove the memory from the vector and link it into
            // the PageSet's linked list.
            mem::forget(vec);
            *list_head = &mut page.header;
            self.page_set.page_count += 1;

            page
        }
    }
}


// UninitializedAllocation /////////////////////////////////////////////////////

/// Nonzero pointer to allocated-but-uninitialized memory.
///
/// This is the type returned by TypedPage::try_fast_alloc().  It exists to
/// help with panic safety. Normally, the caller will dispose of this value by
/// calling its `.commit(val)` method, which populates the allocation with a
/// value and leaves things in a good state. If the thread forgets to do this,
/// or panics instead, this value's destructor will put the never-initialized
/// allocation back onto the freelist, to prevent it from being dropped during
/// the next GC (which would be Undefined Behavior).
///
/// # Safety
///
/// **It is unsafe to GC** while one of these exists!
///
/// Invariant: At any given time, at most one of these objects will exist per
/// heap. (This is a consequence of the safety rule: trying to allocate another
/// object could trigger GC.)
pub struct UninitializedAllocation<U: InHeap> {
    ptr: Pointer<U>
}

impl<U: InHeap> UninitializedAllocation<U> {
    pub fn as_mut(&self) -> *mut U {
        self.ptr.as_mut()
    }

    /// # Safety
    ///
    /// This is safe as long as we've followed all the rules: GC has not occurred
    /// and we have not created any other `UninitializedAllocation`s.
    pub unsafe fn init(self, value: U) -> Pointer<U> {
        debug_assert!(MarkWord::from_ptr(self.ptr, |mw| mw.is_allocated()));
        let ptr = self.ptr;
        ptr::write(ptr.as_mut(), value);
        mem::forget(self);
        ptr
    }
}

impl<U: InHeap> Drop for UninitializedAllocation<U> {
    fn drop(&mut self) {
        // Roll back the allocation.
        unsafe {
            MarkWord::from_ptr(self.ptr, |mw| mw.clear_allocated());
            let page = TypedPage::<U>::find(self.ptr);
            (*page).add_to_free_list(self.as_mut()); // XXX UB if page is aliased
        }
    }
}
