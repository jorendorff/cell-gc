//! GC heap allocator.
//!
//! ### Safety
//!
//! First, a few general observations about safety in Rust.
//!
//! *   If a `mut` reference to a value exists, and other references to the value
//!     also exist, we can definitely crash.
//!
//!     One way it can happen is that the `mut` value is or contains an
//!     `enum`. Using the other reference, we can borrow a refrence to a
//!     current field of the `enum`. Then, using the `mut` reference, we can
//!     assign some other variant to the `enum`, invalidating the reference.
//!     Rust does not detect that the reference is invalid, so when we use it,
//!     we are accessing gibberish.
//!
//!     Another way is if a callback is called while we're in the middle of
//!     mutating a value, while it's in an invalid state. When you read
//!     "callback", think of `deref` and `drop` methods, which are called
//!     implicitly all over the place. These callbacks are normally safe,
//!     because they simply can't have a reference to the value that's in an
//!     intermediate state. But if another reference exists, it might be used
//!     to try to read from the half-mutated value.
//!
//! *   If a data structure contains two paths to a value, under Rust's usual
//!     rules, you can get two `mut` references to that value.
//!
//!     This is why you tend to build ownership trees in Rust and it's why GC
//!     is particularly challenging in Rust: GCs build an arbitrary graph of
//!     values and references.
//!
//! This GC takes the following approach to ensure safety.
//!
//! *   Minimize access to values stored in the heap. For the most part,
//!     application code *never* sees a direct reference to any value that is
//!     physically stored someplace where it's subject to GC.
//!
//! *   Minimize the times when direct references to in-heap values exist at
//!     all, and during these operations, prevent control from escaping to
//!     arbitrary application code.
//!
//! *   Ensure that when any direct references to in-heap values exist, they
//!     obey Rust's rules: for any given value, either only non-`mut`
//!     references, or at most one `mut` reference, exists at a time.
//!
//! Thus we are particularly keen to avoid the possibility of "reentering" the
//! heap, creating new references to in-heap values while others already exist.
//!
//! References to heap values therefore exist only during the following
//! operations:
//!
//! *   Allocation - That is, moving values into the heap. This is safe because
//!     it never triggers any user code at all while heap references exist.
//!
//! * - Heap reads and writes - The only way to do these is via macro-generated
//!     accessors which do not expose references.  Reads call `from_heap()` on
//!     in-heap values, which is dangerous because `from_heap()` receives a
//!     direct reference.  Writes call `drop()`, which is even more dangerous:
//!     (1) it receives a direct `mut` reference; and (2) it leaves in-heap
//!     values uninitialized.
//!
//! *   GC marking - The code for this is all macro-generated.
//!
//! *   GC sweeping - This calls `drop()`, which is dangerous for the reasons
//!     noted above.
//!
//! To make this scheme safe, `from_heap()` and `drop()` must be tightly controlled.
//! `from_heap()` is therefore in an unsafe trait; users are expected to use
//! `#[derive(IntoHeap)]` to autogenerate instances.
//!
//! However, **we leave it up to the user to exercise care with `drop()`.**
//! We suggest *never* implementing `Drop` for a heap type. If you must,
//! avoid reading pointer fields while dropping, and avoid calling into
//! arbitrary code.

use gc_ref::{GcFrozenRef, GcRef};
use marking::{MarkingTracer, mark};
use pages::{self, PageSet, PageSetRef, TypedPage, UninitializedAllocation};
use poison;
use ptr::{Pointer, UntypedPointer};
use signposts;
use ssb::StoreBuffer;
use std::any::TypeId;
use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::hash::{Hasher, BuildHasher};
use std::marker::PhantomData;
use std::mem;
use std::sync::{Arc, Mutex, Weak};
use traits::{InHeap, IntoHeapAllocation, IntoHeapBase, Tracer};

/// A universe in which you can store values that implement
/// `IntoHeapAllocation`. The values are mutable and they can point to each
/// other, in cycles.
pub struct GcHeap {
    /// Map from heap types to the set of pages for that type.
    ///
    /// This owns the page sets, which own the pages. The cleanup when you drop
    /// a `GcHeap` is done by `PageSet::drop`.
    page_sets: HashMap<TypeId, PageSet, BuildTrivialHasher>,

    /// The store buffer remembers the cross generational edges from the tenured
    /// heap into the nursery heap. (Because we use a non-moving, sticky mark
    /// bit implementation of generational GC, these are only conceptually
    /// different heaps, not physically different heaps.)
    nursery_store_buffer: RefCell<StoreBuffer>,

    /// Tracer for the mark phase of GC.
    marking_tracer: Option<MarkingTracer>,

    /// List of pointers that should be unpinned before the next GC cycle. The
    /// `GcFrozenRef` destructor uses this to unpin pointers even though frozen
    /// refs can be sent across thread boundaries.
    ///
    /// The address of this `Mutex` also serves as a unique id for this heap.
    /// `GcFrozenRef` uses it to prevent you from freezing a reference into
    /// one heap, then thawing it in a different heap, you monster.
    dropped_frozen_ptrs: Arc<Mutex<Vec<UntypedPointer>>>,

    /// Used to trigger periodic garbage collection.
    ///
    /// This counter's starting value is `GC_COUNTER_START`. On each allocation,
    /// the counter is decremented. When it hits zero, we do a full mark and
    /// sweep and reset the counter to some new value based on the heap's
    /// current size, `self.alloc_counter`.
    gc_counter: usize,

    /// TODO FITZGEN
    nth_gc: usize,

    /// The number of objects allocated in the heap.
    ///
    /// This counter is incremented by one on every allocation, and decremented
    /// after sweeping by the reported number of objects swept.
    ///
    /// This counter and the `self.gc_counter` are used together to schedule GCs
    /// when the heap grows beyond a certain factor in size. Currently this
    /// factor is about 1.5x, see `Heap::gc`.
    alloc_counter: usize,
}

unsafe impl Send for GcHeap {}

/// An opaque unique id for heaps.
#[derive(Clone)]
pub struct HeapId(Weak<Mutex<Vec<UntypedPointer>>>);

/// What does this do? You'll never guess!
pub type HeapSessionId<'h> = PhantomData<::std::cell::Cell<&'h mut ()>>;

/// Exclusive access to a GC heap.
pub struct GcHeapSession<'h> {
    id: HeapSessionId<'h>,

    /// The heap. It's important that this is an exclusive reference and is
    /// *not* exposed to other code. If other code could call heap.enter() and
    /// create another session at the same time, we could crash.
    heap: &'h mut GcHeap,
}

/// Create a heap, pass it to a callback, then destroy the heap.
///
/// The heap's lifetime is directly tied to this function call, for safety. (So
/// the API is a little wonky --- but how many heaps were you planning on
/// creating?)
pub fn with_heap<R, F>(f: F) -> R
where
    F: for<'h> FnOnce(&mut GcHeapSession<'h>) -> R,
{
    GcHeap::new().enter(f)
}

/// See `Heap::gc_counter` and `Heap::alloc_counter`.
const GC_COUNTER_START: usize = 8192;
const MIN_ALLOCS_BEFORE_GC: usize = GC_COUNTER_START;
const NURSERY_COLLECTIONS_PER_FULL_GC: usize = 7;

impl GcHeap {
    /// Create a new, empty heap.
    pub fn new() -> GcHeap {
        GcHeap {
            page_sets: HashMap::with_hasher(BuildTrivialHasher),
            marking_tracer: Some(MarkingTracer::default()),
            nursery_store_buffer: RefCell::new(StoreBuffer::default()),
            dropped_frozen_ptrs: Arc::new(Mutex::new(Vec::new())),
            gc_counter: GC_COUNTER_START,
            nth_gc: 0,
            alloc_counter: 0,
        }
    }

    /// Get this heap's unique id.
    fn id(&self) -> HeapId {
        HeapId(Arc::downgrade(&self.dropped_frozen_ptrs))
    }

    /// Panic if `heap_id` isn't this heap's id.
    fn check_id(&self, heap_id: HeapId) {
        // Panic if thawing across heaps.
        let heap_id_arc = heap_id
            .0
            .upgrade()
            .expect("can't thaw a reference into a heap that has been dropped");
        assert!(
            Arc::ptr_eq(&heap_id_arc, &self.dropped_frozen_ptrs),
            "can't thaw a frozen reference into a different heap"
        );
    }

    /// Drop a frozen pointer.
    pub(crate) fn drop_frozen_ptr(heap_id: HeapId, ptr: UntypedPointer) {
        // If the heap still exists, add ptr to its internal list of dropped
        // pointers. If not, do nothing; the value was already unpinned and
        // dropped when the heap was dropped.
        if let Some(heap_id_arc) = heap_id.0.upgrade() {
            let mut guard = heap_id_arc.lock().unwrap();
            guard.push(ptr);
        }
    }

    /// Start a session to access this heap.
    ///
    /// You need a `GcHeapSession` in order to do anything interesting with a
    /// heap.  Each heap has either 0 or 1 `GcHeapSession` at a time, and a
    /// `GcHeapSession` is bound to a stack lifetime, so Rust can enforce
    /// safety rules.
    ///
    /// It would be safe to make this method public, but it's a pain in
    /// practice. You'll want to pass a `&mut GcHeapSession<'h>` around, not a
    /// `GcHeapSession<'h>`, since `GcHeapSession` is not `Copy`. Use `enter`.
    fn open<'h>(&'h mut self) -> GcHeapSession<'h> {
        GcHeapSession {
            id: PhantomData,
            heap: self,
        }
    }

    /// Run some code using this GcHeap.
    ///
    /// # Example
    ///
    ///     use cell_gc::{GcHeap, GcLeaf};
    ///
    ///     let mut heap = GcHeap::new();
    ///     heap.enter(|hs| {
    ///         // ... hs.alloc(MyHeapStruct { ... }) ...
    ///         # hs.force_gc();
    ///     });
    ///
    pub fn enter<R, F>(&mut self, f: F) -> R
    where
        F: for<'h> FnOnce(&mut GcHeapSession<'h>) -> R,
    {
        f(&mut self.open())
    }

    /// Get the GC heap that this pointer was allocated in.
    ///
    /// ### Safety
    ///
    /// The give pointer must point to a valid allocation inside a live GC page,
    /// otherwise random memory will be dereferenced.
    pub unsafe fn from_allocation<'h, T: IntoHeapAllocation<'h>>(
        ptr: Pointer<T::In>,
    ) -> *const GcHeap {
        (*TypedPage::find(ptr)).header.heap
    }

    fn take_marking_tracer(&mut self) -> MarkingTracer {
        self.marking_tracer.take().expect("attempted nested GC")
    }

    fn replace_marking_tracer(&mut self, tracer: MarkingTracer) {
        assert!(self.marking_tracer.is_none());
        assert!(tracer.mark_stack_is_empty());
        self.marking_tracer = Some(tracer);
    }

    /// Run the given function with the marking tracer.
    ///
    /// The marking tracer is taken out of the heap and replaced again so we can
    /// have two independent borrows of the heap and the marking tracer and the
    /// same time.
    pub(crate) fn with_marking_tracer<F, O>(&mut self, mut f: F) -> O
    where
        F: FnMut(&mut MarkingTracer) -> O,
    {
        let mut tracer = self.take_marking_tracer();
        let retval = f(&mut tracer);
        self.replace_marking_tracer(tracer);
        retval
    }

    /// Invoke the given function with the nursery store buffer.
    pub(crate) fn with_nursery_store_buffer<F, O>(&self, mut f: F) -> O
    where
        F: FnMut(&mut StoreBuffer) -> O,
    {
        let mut store_buffer = self.nursery_store_buffer.borrow_mut();
        f(&mut *store_buffer)
    }

    /// Clear all mark bits in preparation for GC.
    ///
    /// # Safety
    ///
    /// This must be called only at the beginning of a GC cycle.
    pub(crate) unsafe fn clear_mark_bits(&mut self, roots: &mut Vec<UntypedPointer>) {
        for page_set in self.page_sets.values_mut() {
            page_set.clear_mark_bits(roots);
        }
    }

    /// Walk the pages and collect all of the roots.
    pub(crate) fn mark_and_collect_roots(&mut self, roots: &mut Vec<UntypedPointer>) {
        for page_set in self.page_sets.values_mut() {
            page_set.mark_and_collect_roots(roots);
        }
    }

    fn unpin_dropped_ptrs(&mut self) {
        let dropped_ptrs = {
            let mut guard = self.dropped_frozen_ptrs.lock().unwrap();
            let main: &mut Vec<UntypedPointer> = &mut guard;
            let mut tmp: Vec<UntypedPointer> = Vec::new();
            mem::swap(main, &mut tmp);
            tmp
        };

        for p in dropped_ptrs {
            unsafe {
                pages::unpin_untyped(p);
            }
        }
    }

    fn gc_impl(&mut self, nursery: bool) {
        println!("GC: nursery? {}", nursery);
        self.nth_gc += 1;

        self.unpin_dropped_ptrs();

        let mut roots = vec![];
        if nursery {
            self.mark_and_collect_roots(&mut roots);
            let mut store_buffer = self.nursery_store_buffer.borrow_mut();
            unsafe {
                roots.extend(store_buffer.drain());
            }
        } else {
            unsafe {
                let _sp = signposts::ClearMarkBits::new();
                self.clear_mark_bits(&mut roots);
            }
        }

        mark(self, roots);

        let _sp = signposts::Sweeping::new();

        let mut num_swept = 0;
        for page_set in self.page_sets.values_mut() {
            unsafe {
                num_swept += page_set.sweep();
            }
        }

        assert!(
            num_swept <= self.alloc_counter,
            "Should never sweep more objects than are currently allocated"
        );
        self.alloc_counter -= num_swept;

        println!("num_swept = {}", num_swept);

        println!("gc_counter before = {}", self.gc_counter);
        // self.gc_counter = if nursery {
        //     // Delay full GC for another 4x the number of objects that this
        //     // nursery collection swept. Therefore, if nursery collection is
        //     // really effective, full GC is very delayed, and if nursery
        //     // collection is ineffective, we will do a full GC sooner.
        //     cmp::max(self.gc_counter + num_swept * 4, self.alloc_counter)
        // } else {
        //     // Schedule a GC for when the heap reaches 4x its current
        //     // size. Unless the heap is really small, in which case we don't
        //     // want to set the gc counter get to some ridiculously low number.
        //     cmp::max(self.alloc_counter * 3, MIN_ALLOCS_BEFORE_GC)
        // };
        self.gc_counter = cmp::max(self.alloc_counter * 3, MIN_ALLOCS_BEFORE_GC);

        println!("gc_counter after = {}", self.gc_counter);
    }

    fn scheduled_gc(&mut self) {
        if self.nth_gc % (NURSERY_COLLECTIONS_PER_FULL_GC + 1) == NURSERY_COLLECTIONS_PER_FULL_GC {
            self.tenured_gc();
        } else {
            self.nursery_gc();
        }
    }

    /// Do a gc of the full heap.
    fn tenured_gc(&mut self) {
        self.gc_impl(false);
    }

    /// Do a nursery collection.
    fn nursery_gc(&mut self) {
        self.gc_impl(true);
    }

    fn is_empty(&self) -> bool {
        self.page_sets
            .values()
            .all(|page_set| page_set.all_pages_are_empty())
    }
}

// GcHeap does not need its own destructor, since PageSet's destructor does all
// the work; but when building with signposts, we want to measure how long this
// takes.
impl Drop for GcHeap {
    fn drop(&mut self) {
        let _sp = signposts::Dropping::new();
        self.page_sets.clear();
    }
}

impl<'h> GcHeapSession<'h> {
    fn get_page_set<'a, U: InHeap>(&'a mut self) -> PageSetRef<'a, U> {
        let key = pages::heap_type_id::<U>();
        let heap: *mut GcHeap = self.heap;
        self.heap
            .page_sets
            .entry(key)
            .or_insert_with(|| unsafe { PageSet::new::<U>(heap) })
            .downcast_mut()
    }

    /// Set (or unset) the limit on the number of pages that can be used to
    /// allocate values of type `T` in this heap. By default, no limit is set.
    ///
    /// See `try_alloc` for more.
    ///
    /// If there are already at least `limit` pages for `T` values, this may have no effect;
    /// it doesn't cause pages to be freed.
    pub fn set_page_limit<T: IntoHeapAllocation<'h>>(&mut self, limit: Option<usize>) {
        self.get_page_set::<T::In>().set_page_limit(limit);
    }

    /// Allocate memory, moving `value` into the heap.
    ///
    /// If a limit has previously been set using `set_page_limit`, and we run
    /// up against the limit (already have at least that many pages for `T`
    /// values, and they are all full of live values), `try_alloc` first
    /// attempts to free some memory by doing garbage collection. If that
    /// doesn't work, `try_alloc` returns `None`.
    pub fn try_alloc<T: IntoHeapAllocation<'h>>(&mut self, value: T) -> Option<T::Ref> {
        unsafe {
            if let Some(allocation) = self.try_fast_alloc::<T>() {
                let u = value.into_heap();
                let ptr = allocation.init(u);
                Some(T::wrap_gc_ref(GcRef::new(ptr)))
            } else {
                self.try_slow_alloc(value)
            }
        }
    }

    /// Allocate space for a `T::In` value without performing GC or doing any
    /// system calls, if possible.
    ///
    /// # Safety
    ///
    /// Safe as long as GC isn't currently happening and no
    /// `UninitializedAllocation`s already exist in this heap.
    unsafe fn try_fast_alloc<T: IntoHeapAllocation<'h>>(&mut self) -> Option<UninitializedAllocation<T::In>> {
        self.heap.gc_counter = self.heap.gc_counter.saturating_sub(1);
        self.get_page_set::<T::In>().try_fast_alloc()
            .map(|p| {
                self.heap.alloc_counter += 1;
                p
            })
    }

    fn try_slow_alloc<T: IntoHeapAllocation<'h>>(&mut self, value: T) -> Option<T::Ref> {
        self.heap.gc_counter = self.heap.gc_counter.saturating_sub(1);
        if self.heap.gc_counter == 0 {
            self.heap.scheduled_gc();
        }
        unsafe {
            let allocation = match self.get_page_set::<T::In>().try_alloc() {
                Some(p) => p,
                None => {
                    self.heap.tenured_gc();
                    match self.get_page_set::<T::In>().try_alloc() {
                        Some(p) => p,
                        None => return None,
                    }
                }
            };

            self.heap.alloc_counter += 1;
            let u = value.into_heap();
            let p = allocation.init(u);
            let gc_ref = T::wrap_gc_ref(GcRef::new(p));
            Some(gc_ref)
        }
    }

    /// Allocate memory, moving `T` into the heap. This may cause garbage collection.
    ///
    /// # Panics
    ///
    /// If a page limit has been set, all pages are full, and GC fails to shake
    /// anything loose.
    pub fn alloc<T: IntoHeapAllocation<'h>>(&mut self, value: T) -> T::Ref {
        self.try_alloc(value)
            .expect("out of memory (gc did not collect anything)")
    }

    /// Do garbage collection.
    pub fn force_gc(&mut self) {
        self.heap.tenured_gc();
    }

    /// Freeze a reference to a GC thing so that it can outlive the current GC
    /// heap session, and be thawed at another time.
    pub fn freeze<T: IntoHeapAllocation<'h>>(&self, t: T::Ref) -> GcFrozenRef<T> {
        GcFrozenRef::new(&self, t)
    }

    /// Thaw a frozen GC reference back into the current GC heap session, so
    /// that its referent can be accessed again.
    pub fn thaw<T: IntoHeapAllocation<'h>>(&self, t: GcFrozenRef<T>) -> T::Ref {
        T::wrap_gc_ref(t.thaw(&self))
    }

    /// Get this session's GC heap's ID.
    pub(crate) fn heap_id(&self) -> HeapId {
        self.heap.id()
    }

    /// Assert that this session's GC heap matches the given ID's heap.
    pub(crate) fn check_heap_id(&self, heap_id: HeapId) {
        self.heap.check_id(heap_id);
    }

    /// Returns true if all allocations have been collected. This implies that
    /// no `GcRef`s into the heap exist. You may need to call `hs.force_gc()`
    /// before this, to get predictable results.
    ///
    /// This method is provided for testing only and may disappear without
    /// warning.
    #[doc(hidden)]
    pub fn is_empty(&self) -> bool {
        self.heap.is_empty()
    }
}

// TrivialHasher ///////////////////////////////////////////////////////////////

/// A hasher that does minimal work. This is used exclusively for hashing
/// `TypeId` values in the map `GcHeap::page_sets`. It's measurably faster than
/// the default hasher for this purpose.
struct TrivialHasher {
    state: u64
}

impl Hasher for TrivialHasher {
    #[inline]
    fn finish(&self) -> u64 {
        // Mix the bits a little.
        //
        // TypeId hashing is undocumented. In Rust 1.21, it calls
        // `.write_u64()`, always passing an even number.
        //
        // Whatever .finish() returns will be bitmasked and used as a hash
        // table index without further ado. We're responsible for making sure
        // each bit of that word has some entropy in it.
        //
        // The magic multiplicand here is an arbitrary 64-bit integer with the
        // most and least significant bits set and a mix of zeros and ones;
        // those properties maximize our tendency to spread whatever entropy we
        // have in some bits of self.state throughout the output bits. Not bad,
        // but there is a limit to the magic a single multiplication can work:
        // since `self.state` is always even, the result is always even. So,
        // shift the always-zero bit to someplace where it won't be used.
        self.state.wrapping_mul(0xd171_35cf_f019_a245).rotate_right(8)
    }

    /// Unused. If the standard library implementation of Hash for TypeId
    /// changes, we definitely want to know about it, so panic.
    fn write(&mut self, _bytes: &[u8]) {
        panic!("unexpected TypeId hashing");
    }

    fn write_u64(&mut self, u: u64) {
        self.state = u;
    }
}

struct BuildTrivialHasher;

impl BuildHasher for BuildTrivialHasher {
    type Hasher = TrivialHasher;
    fn build_hasher(&self) -> TrivialHasher {
        TrivialHasher { state: 0 }
    }
}

/// TODO FITZGEN
pub trait CanGc {
    /// TODO FITZGEN
    fn can_gc() -> bool;
}

/// TODO FITZGEN
pub enum NoGc {}

impl CanGc for NoGc {
    #[inline(always)]
    fn can_gc() -> bool {
        false
    }
}

/// TODO FITZGEN
pub enum YesGc {}

impl CanGc for YesGc {
    #[inline(always)]
    fn can_gc() -> bool {
        true
    }
}

/// The post-write barrier to remember cross generational tenured to nursery
/// edges.
///
/// target.edge = written;
#[inline(always)]
pub unsafe fn post_write_barrier<'h, T, U, AllowGc>(target: &Pointer<T::In>, written: &U::In)
where
    T: IntoHeapAllocation<'h>,
    U: IntoHeapBase,
    AllowGc: CanGc,
{
    poison::assert_is_not_poisoned(target.as_raw());
    if pages::get_mark_bit(*target) {
        post_write_barrier_slow::<T, U, AllowGc>(target, written);
    }
}

#[inline(never)]
unsafe fn post_write_barrier_slow<'h, T, U, AllowGc>(target: &Pointer<T::In>, written: &U::In)
where
    T: IntoHeapAllocation<'h>,
    U: IntoHeapBase,
    AllowGc: CanGc,
{
    struct Remember<'a> {
        store_buffer: &'a mut StoreBuffer,
        need_gc: bool,
    }

    impl<'a> Tracer for Remember<'a> {
        fn visit<U: InHeap>(&mut self, ptr: Pointer<U>) {
            unsafe {
                poison::assert_is_not_poisoned(ptr.as_raw());
                if !pages::get_mark_bit(ptr) {
                    self.need_gc |= self.store_buffer.insert::<U>(ptr);
                }
            }
        }
    }

    poison::assert_is_not_poisoned(target.as_raw());
    let heap = GcHeap::from_allocation::<T>(*target) as *mut GcHeap;

    let need_gc = (*heap).with_nursery_store_buffer(|sb| {
        let mut remember = Remember {
            store_buffer: sb,
            need_gc: false,
        };

        written.trace(&mut remember);

        remember.need_gc
    });

    if AllowGc::can_gc() && need_gc {
        (*heap).nursery_gc();
    }
}
