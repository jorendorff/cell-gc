//! Marking heap tracer and mark stack implementation.

use heap::GcHeap;
use pages::PageHeader;
use ptr::{Pointer, UntypedPointer};
use traits::{IntoHeapAllocation, Tracer};


/// Perform all the marking for a collection.
pub fn mark<'h>(heap: &mut GcHeap) {
    unsafe {
        heap.clear_mark_bits();
    }

    heap.with_marking_tracer(|heap, mut tracer| {
        heap.each_pin(|ptr| unsafe {
            (*PageHeader::find(ptr)).mark(ptr, &mut tracer);
        });

        tracer.mark_to_fix_point();
    });
}

/// The marking tracer is a `Tracer` that visits every edge in the live heap
/// graph and sets its mark bit.
///
/// A `MarkingTracer` has a mark stack, but because the mark stack is a
/// heterogenous collection, we need to perform an indirect call to trace and
/// mark its entries. Compared to a direct trace call, this is expensive. We
/// can't completely avoid having a mark stack, because then we would only make
/// recursive `trace` calls and we could blow the stack with long singly linked
/// lists. We adopt a hybrid approach that avoids most indirect calls by keeping
/// track of "fuel". The tracer's fuel denotes how many more recursive `trace`
/// calls we can make directly. When fuel runs out, we push new edges onto the
/// mark stack which we will process after we unwind the call stack. The result
/// is that we make one indirect call (via `mark_entry_point`) for each entry in
/// the mark stack, and transitive `trace` calls (which are the majority) never
/// involve indirection.
pub struct MarkingTracer {
    fuel: usize,
    mark_stack: Vec<UntypedPointer>,
}

// TODO: Choose a better default value based on the average size of a trace
// function's stack frame and average size of the stack on typical platforms or
// something...
const DEFAULT_FUEL: usize = 100;

impl<'h> Default for MarkingTracer {
    fn default() -> MarkingTracer {
        MarkingTracer::new(DEFAULT_FUEL)
    }
}

impl<'h> MarkingTracer {
    /// Construct a new marking tracer, able to make `fuel` direct, recursive
    /// `trace` calls.
    pub fn new(fuel: usize) -> MarkingTracer {
        assert!(fuel > 0);
        MarkingTracer {
            fuel: fuel,
            mark_stack: Default::default(),
        }
    }

    /// Run the marking phase until we reach a fix-point, when the mark stack is
    /// empty.
    pub fn mark_to_fix_point(&mut self) {
        while let Some(ptr) = self.mark_stack.pop() {
            unsafe {
                (*PageHeader::find(ptr)).mark(ptr, self);
            }
        }
    }

    /// Is the mark stack empty?
    pub fn mark_stack_is_empty(&self) -> bool {
        self.mark_stack.is_empty()
    }
}

impl Tracer for MarkingTracer {
    fn visit<'h, T>(&mut self, ptr: Pointer<T::In>)
    where
        T: IntoHeapAllocation<'h>,
    {
        let is_marked = unsafe { GcHeap::get_mark_bit::<T>(ptr) };
        if is_marked {
            return;
        }

        unsafe {
            GcHeap::set_mark_bit::<T>(ptr);
        }

        if self.fuel == 0 {
            // Out of fuel. We don't want to blow the stack, so save this thing
            // for later, after we unwind.
            self.mark_stack.push(ptr.into());
            return;
        }

        // Keep going with the monomorphized fast path for marking.
        self.fuel -= 1;
        unsafe {
            T::trace(ptr.as_ref(), self);
        }
        self.fuel += 1;
    }
}
