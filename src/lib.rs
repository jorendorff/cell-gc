use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;

// What does this do? You'll never guess!
type HeapId<'a> = PhantomData<std::cell::Cell<&'a mut ()>>;

// The heap is (for now) just a big Vec of Pairs
pub struct Heap<'a> {
    id: HeapId<'a>,
    storage: Vec<Markable<PairStorage<'a>>>,
    freelist: *mut Markable<PairStorage<'a>>,
    pins: RefCell<HashMap<*mut Markable<PairStorage<'a>>, usize>>
}

pub const HEAP_SIZE: usize = 10000;

/// Create a heap, pass it to a callback, then destroy the heap.
///
/// The heap's lifetime is directly tied to this function call, for safety. (So
/// the API is a little wonky --- but how many heaps were you planning on
/// creating?)
pub fn with_heap<F, O>(f: F) -> O
    where F: for<'a> FnOnce(&mut Heap<'a>) -> O
{
    f(&mut Heap::new())
}

/// Trait implemented by all types that can be stored directly in the GC heap:
/// the `Storage` types associated with any `HeapInline` or `HeapRef` type.
///
/// XXX maybe this should not be its own trait - fold into HeapInline?
///
unsafe trait Mark<'a> {
    unsafe fn mark(ptr: *mut Self);
}

struct Markable<T> {
    marked: bool,
    value: T
}

/// Trait implemented by all types that can be stored in fields of structs (or,
/// eventually, elements of GCVecs) that are stored in the GC heap.
unsafe trait HeapInline<'a> {
    /// The type of the value when it is physically stored in the heap.
    type Storage;

    /// Extract the value from the heap. Do not under any circumstances call
    /// this.  It is for macro-generated code to call; it is impossible for
    /// ordinary users to call this safely, because `ptr` must be a direct,
    /// unwrapped reference to a value stored in the GC heap, which ordinary
    /// users cannot obtain.
    ///
    /// This turns any raw pointers in the `Storage` value into safe
    /// references, so while it's a dangerous function, the result of a correct
    /// call can be safely handed out to user code.
    ///
    unsafe fn from_heap(heap: &Heap<'a>, ptr: &Self::Storage) -> Self;

    /// Convert the value to the form it should have in the heap.
    /// This is for macro-generated code to call.
    fn to_heap(self) -> Self::Storage;
}

impl<'a> Heap<'a> {
    fn new() -> Heap<'a> {
        // Allocate with the full capacity so that allocated Pairs never move
        let mut h = Heap {
            id: PhantomData,
            storage: Vec::with_capacity(HEAP_SIZE),
            freelist: std::ptr::null_mut(),
            pins: RefCell::new(HashMap::new())
        };
        for i in 0 .. HEAP_SIZE {
            h.storage.push(Markable {
                marked: false,
                value: PairStorage {
                    head: ValueStorage::Null,
                    tail: ValueStorage::Null
                }
            });
            let p = &mut h.storage[i] as *mut Markable<PairStorage<'a>>;
            unsafe {
                h.add_to_free_list(p);
            }
        }
        h
    }

    fn pin(&self, p: *mut Markable<PairStorage<'a>>) {
        let mut pins = self.pins.borrow_mut();
        let entry = pins.entry(p).or_insert(0);
        *entry += 1;
    }

    fn unpin(&self, p: *mut Markable<PairStorage<'a>>) {
        let mut pins = self.pins.borrow_mut();
        if {
            let entry = pins.entry(p).or_insert(0);
            assert!(*entry != 0);
            *entry -= 1;
            *entry == 0
        } {
            pins.remove(&p);
        }
    }

    unsafe fn add_to_free_list(&mut self, p: *mut Markable<PairStorage<'a>>) {
        let listp: *mut *mut Markable<PairStorage<'a>> = std::mem::transmute(p);
        *listp = self.freelist;
        assert_eq!(*listp, self.freelist);
        self.freelist = p;
    }

    pub fn try_alloc(&mut self, pair: (Value<'a>, Value<'a>)) -> Option<Pair<'a>> {
        if self.freelist.is_null() {
            unsafe {
                self.gc();
            }
            if self.freelist.is_null() {
                return None;
            }
        }

        let p = self.freelist;
        unsafe {
            let listp: *mut *mut Markable<PairStorage<'a>> = std::mem::transmute(p);
            self.freelist = *listp;

            let (h, t) = pair;
            *p = Markable {
                marked: false,
                value: PairStorage {
                    head: h.to_heap(),
                    tail: t.to_heap()
                }
            };
            Some(Pair(PinnedRef::new(self, p)))
        }
    }

    pub fn alloc(&mut self, pair: (Value<'a>, Value<'a>)) -> Pair<'a> {
        self.try_alloc(pair).expect("out of memory (gc did not collect anything)")
    }

    pub fn alloc_null(&mut self) -> Pair<'a> {
        self.alloc((Value::Null, Value::Null))
    }

    unsafe fn gc(&mut self) {
        // clear mark bits
        for p in &mut self.storage {
            p.marked = false;
        }

        // mark phase
        for (&p, _) in self.pins.borrow().iter() {
            Mark::mark(p);
        }

        // sweep phase
        self.freelist = std::ptr::null_mut();
        for i in 0 .. HEAP_SIZE {
            let p = &mut self.storage[i] as *mut Markable<PairStorage<'a>>;
            if !(*p).marked {
                self.add_to_free_list(p);
            }
        }
    }

    #[cfg(test)]
    pub fn force_gc(&mut self) {
        unsafe { self.gc(); }
    }
}

struct PinnedRef<'a, T> {
    ptr: *mut T,
    heap: *const Heap<'a>,
    heap_id: HeapId<'a>
}

impl<'a, T> PinnedRef<'a, T> {
    unsafe fn new(heap: &Heap<'a>, p: *mut T) -> PinnedRef<'a, T> {
        heap.pin(p as *mut Markable<PairStorage<'a>>);  // XXX BOGUS
        PinnedRef {
            ptr: p,
            heap: heap as *const Heap<'a>,
            heap_id: heap.id
        }
    }
}

impl<'a, T> Drop for PinnedRef<'a, T> {
    fn drop(&mut self) {
        unsafe {
            (*self.heap).unpin(self.ptr as *mut Markable<PairStorage<'a>>);  // XXX BOGUS
        }
    }
}

impl<'a, T> Clone for PinnedRef<'a, T> {
    fn clone(&self) -> PinnedRef<'a, T> {
        let &PinnedRef { ptr, heap, heap_id } = self;
        unsafe {
            (*heap).pin(ptr as *mut Markable<PairStorage<'a>>);  // XXX BOGUS
        }
        PinnedRef {
            ptr: ptr,
            heap: heap,
            heap_id: heap_id
        }
    }
}

impl<'a, T> ::std::fmt::Debug for PinnedRef<'a, T> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "PinnedRef {{ ptr: {:p} }}", self.ptr)
    }
}

impl<'a, T> PartialEq for PinnedRef<'a, T> {
    fn eq(&self, other: &PinnedRef<'a, T>) -> bool {
        self.ptr == other.ptr
    }
}

impl<'a, T> Eq for PinnedRef<'a, T> {}



// === Pair, the reference type

pub trait GCRef {
    #[cfg(test)]
    fn address(&self) -> usize;
}

trait TransplantLifetime<'a> {
    type Result: HeapInline<'a>;
}

impl<'a, T: Copy + 'static> TransplantLifetime<'a> for T {
    type Result = T;
}

macro_rules! gc_ref_type {
    (pub struct $ref_type: ident / $storage_type: ident {
        $($field_name: ident / $field_setter_name: ident : $field_type: ty),*
    }) => {
        struct $storage_type <'a> {
            $($field_name: <<$field_type as TransplantLifetime<'a>>::Result as HeapInline<'a>>::Storage,)*
        }

        unsafe impl<'a> Mark<'a> for Markable<$storage_type<'a>> {
            unsafe fn mark(ptr: *mut Markable<$storage_type<'a>>) {
                if !ptr.is_null() && !(*ptr).marked {
                    (*ptr).marked = true;
                    $(
                        Mark::mark(&mut (*ptr).value.$field_name
                                   as *mut <<$field_type as TransplantLifetime<'a>>::Result as HeapInline<'a>>::Storage);
                    )*
                }
            }
        }

        #[allow(raw_pointer_derive)]
        #[derive(Clone, Debug, PartialEq)]
        pub struct $ref_type<'a>(PinnedRef<'a, Markable<$storage_type<'a>>>);

        impl<'a> $ref_type<'a> {
            $(
                pub fn $field_name(&self) -> <$field_type as TransplantLifetime<'a>>::Result {
                    let ptr = self.0.ptr;
                    unsafe {
                        <<$field_type as TransplantLifetime<'a>>::Result as HeapInline<'a>>::from_heap(
                            &*self.0.heap,
                            &(*ptr).value.$field_name)
                    }
                }

                pub fn $field_setter_name(&self, v: <$field_type as TransplantLifetime<'a>>::Result) {
                    let ptr = self.0.ptr;
                    unsafe {
                        (*ptr).value.$field_name =
                            <<$field_type as TransplantLifetime<'a>>::Result as HeapInline<'a>>::to_heap(v);
                    }
                }
            )*
        }

        unsafe impl<'a> HeapInline<'a> for $ref_type<'a> {
            type Storage = *mut Markable<$storage_type<'a>>;

            fn to_heap(self) -> Self::Storage {
                self.0.ptr
            }

            unsafe fn from_heap(heap: &Heap<'a>, v: &Self::Storage) -> Self {
                $ref_type(PinnedRef::new(heap, *v))
            }
        }

        impl<'a> GCRef for $ref_type<'a> {
            #[cfg(test)]
            fn address(&self) -> usize {
                unsafe { std::mem::transmute(self.0.ptr) }
            }
        }
    }
}

gc_ref_type! {
    pub struct Pair / PairStorage {
        head / set_head: Value<'static>,
        tail / set_tail: Value<'static>
    }
}


// === Values (a heap-inline enum)

// Values inside the heap (GC heap-to-heap cross-references) (private)
enum ValueStorage<'a> {
    Null,
    Int(i32),
    Str(Rc<String>),
    Pair(*mut Markable<PairStorage<'a>>)
}

unsafe impl<'a> Mark<'a> for ValueStorage<'a> {
    unsafe fn mark(ptr: *mut ValueStorage<'a>) {
        match *ptr {
            ValueStorage::Pair(p) => Mark::mark(p),
            _ => {}
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    Null,
    Int(i32),
    Str(Rc<String>),  // <-- equality is by value
    Pair(Pair<'a>)  // <-- equality is by pointer
}

unsafe impl<'a> HeapInline<'a> for Value<'a> {
    type Storage = ValueStorage<'a>;

    fn to_heap(self) -> ValueStorage<'a> {
        match self {
            Value::Null => ValueStorage::Null,
            Value::Int(n) => ValueStorage::Int(n),
            Value::Str(rcstr) => ValueStorage::Str(rcstr),
            Value::Pair(pair) => ValueStorage::Pair(HeapInline::<'a>::to_heap(pair))
        }
    }

    unsafe fn from_heap(heap: &Heap<'a>, v: &ValueStorage<'a>) -> Value<'a> {
        match v {
            &ValueStorage::Null => Value::Null,
            &ValueStorage::Int(n) => Value::Int(n),
            &ValueStorage::Str(ref rcstr) => Value::Str(rcstr.clone()),
            &ValueStorage::Pair(ref ptr) => Value::Pair(HeapInline::<'a>::from_heap(heap, ptr))
        }
    }
}

impl<'a, 'b> TransplantLifetime<'a> for Value<'b> {
    type Result = Value<'a>;
}

unsafe impl<'a, T: Copy + 'static> Mark<'a> for T {
    unsafe fn mark(_ptr: *mut T) {}
}

unsafe impl<'a, T: Copy + 'static> HeapInline<'a> for T {
    type Storage = Self;

    fn to_heap(self) -> T { self }

    unsafe fn from_heap(_heap: &Heap<'a>, v: &T) -> T { *v }
}


#[cfg(test)]
mod test;
