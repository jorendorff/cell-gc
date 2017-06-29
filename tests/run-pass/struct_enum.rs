//! Struct-like enum variants are supported.

#[macro_use] extern crate cell_gc;
#[macro_use] extern crate cell_gc_derive;

use cell_gc::collections::VecRef;

#[derive(IntoHeap)]
struct ThingBox<'h> {
    thing: Thing<'h>
}

#[derive(IntoHeap)]
enum Thing<'h> {
    Zero,
    One { it: ThingBoxRef<'h> },
    Two { left: ThingBoxRef<'h>, right: ThingBoxRef<'h> }
}

fn main() {
    cell_gc::with_heap(|hs| {
        let zero = hs.alloc(ThingBox { thing: Thing::Zero });
        let one = hs.alloc(ThingBox {
            thing: Thing::One { it: zero.clone() }
        });
        let two = hs.alloc(ThingBox {
            thing: Thing::Two { left: zero.clone(), right: one.clone() }
        });
        let v: VecRef<ThingBoxRef> = hs.alloc(vec![zero, one, two]);

        fn log<'h>(out: &mut String, r: ThingBoxRef<'h>) {
            match r.thing() {
                Thing::Zero => out.push('*'),
                Thing::One { it } => {
                    out.push('(');
                    log(out, it);
                    out.push(')');
                }
                Thing::Two { left, right } => {
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
