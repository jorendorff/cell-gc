//! Basic tuple-like enum variants are supported.

#[macro_use] extern crate cell_gc;

use cell_gc::collections::VecRef;

gc_heap_type! {
    struct ThingBox / ThingBoxRef / ThingBoxInHeap <'h> {
        thing / set_thing: Thing<'h>
    }
}

gc_heap_type! {
    enum Thing / ThingInHeap <'h> {
        Zero,
        One(ThingBoxRef<'h>),
        Two(ThingBoxRef<'h>, ThingBoxRef<'h>)
    }
}

fn main() {
    cell_gc::with_heap(|heap| {
        let zero = heap.alloc(ThingBox { thing: Thing::Zero });
        let one = heap.alloc(ThingBox { thing: Thing::One(zero.clone()) });
        let two = heap.alloc(ThingBox { thing: Thing::Two(zero.clone(), one.clone()) });
        let v: VecRef<ThingBoxRef> = heap.alloc(vec![zero, one, two]);

        fn log<'h>(out: &mut String, r: ThingBoxRef<'h>) {
            match r.thing() {
                Thing::Zero => out.push('*'),
                Thing::One(it) => {
                    out.push('(');
                    log(out, it);
                    out.push(')');
                }
                Thing::Two(left, right) => {
                    out.push('[');
                    log(out, left);
                    log(out, right);
                    out.push(']');
                }
            }
        }

        let mut out = String::new();
        for i in 0 .. 3 {
            log(&mut out, v.get(i));
        }
        assert_eq!(out, "*(*)[*(*)]");
    });
}
