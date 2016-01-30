//! Static strings can be stored in the heap.

#[macro_use] extern crate cell_gc;

gc_heap_type! {
    struct Philosopher / RefPhilosopher / InHeapPhilosopher <'h> {
        name / set_name: &'static str,
        teacher / set_teacher: Option<RefPhilosopher<'h>>
    }
}

fn main() {
    cell_gc::with_heap(|heap| {
        let s = heap.alloc(Philosopher {
            name: "Socrates",
            teacher: None
        });
        let p = heap.alloc(Philosopher {
            name: "Plato",
            teacher: Some(s.clone())
        });
        let a = heap.alloc(Philosopher {
            name: "Aristotle",
            teacher: Some(p.clone())
        });
        assert_eq!(a.name(), "Aristotle");
        assert_eq!(s.name(), "Socrates");
        assert_eq!(a.teacher().unwrap().name(), "Plato");
        assert_eq!(a.teacher().unwrap().teacher().unwrap().name(), "Socrates");
    });
}
