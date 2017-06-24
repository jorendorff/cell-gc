use heap::{Heap, HeapId};
use pages::PageHeader;
use traits::{IntoHeap, Tracer};


pub fn mark<'h>(heap: &mut Heap<'h>) {
    heap.clear_mark_bits();

    heap.with_marking_tracer(|heap, mut tracer| {
        heap.each_pin(|ptr| {
            unsafe {
                (*PageHeader::<'h>::find(ptr)).mark(ptr, &mut tracer);
            }
        });

        tracer.mark_to_fix_point();
    });
}

/// TODO FITZGEN
pub struct MarkingTracer<'h> {
    _heap: HeapId<'h>,
    fuel: usize,
    mark_stack: Vec<*const ()>,
}

// TODO: Choose a better default value based on the average size of a trace
// function's stack frame or something...
const DEFAULT_FUEL: usize = 100;

impl<'h> Default for MarkingTracer<'h> {
    fn default() -> MarkingTracer<'h> {
        MarkingTracer::new(DEFAULT_FUEL)
    }
}

impl<'h> MarkingTracer<'h> {
    /// TODO FITZGEN
    pub fn new(fuel: usize) -> MarkingTracer<'h> {
        assert!(fuel > 0);
        MarkingTracer {
            _heap: Default::default(),
            fuel: fuel,
            mark_stack: Default::default(),
        }
    }

    /// TODO FITZGEN
    pub fn mark_to_fix_point(&mut self) {
        while let Some(ptr) = self.mark_stack.pop() {
            unsafe {
                (*PageHeader::<'h>::find(ptr as *mut _)).mark(ptr, self);
            }
        }
    }
}

impl<'h> Tracer<'h> for MarkingTracer<'h> {
    fn visit<T>(&mut self, thing: &T::In) where T: IntoHeap<'h> {
        let is_marked = unsafe { Heap::get_mark_bit::<T>(thing) };
        if is_marked {
            return;
        }

        unsafe {
            Heap::set_mark_bit::<T>(thing);
        }

        if self.fuel == 0 {
            // Out of fuel. We don't want to blow the stack, so save this thing
            // for later, after we unwind.
            self.mark_stack.push(thing as *const T::In as *const _);
            return;
        }

        // Keep going with the monomorphized fast path for marking.
        self.fuel -= 1;
        unsafe {
            T::trace(thing, self);
        }
        self.fuel += 1;
    }
}
