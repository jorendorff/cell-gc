//! Static strings can be stored in the heap.

extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;

#[derive(IntoHeap)]
struct Philosopher<'h> {
    name: &'static str,
    teacher: Option<PhilosopherRef<'h>>,
}

#[test]
fn strings_in_heap() {
    cell_gc::with_heap(|hs| {
        let s = hs.alloc(Philosopher {
            name: "Socrates",
            teacher: None,
        });
        let p = hs.alloc(Philosopher {
            name: "Plato",
            teacher: Some(s.clone()),
        });
        let a = hs.alloc(Philosopher {
            name: "Aristotle",
            teacher: Some(p.clone()),
        });
        assert_eq!(a.name(), "Aristotle");
        assert_eq!(s.name(), "Socrates");
        assert_eq!(a.teacher().unwrap().name(), "Plato");
        assert_eq!(a.teacher().unwrap().teacher().unwrap().name(), "Socrates");
    });
}
