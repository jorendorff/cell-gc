//! The GC can work with objects that take up most of a page.

extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;

type Big32 = (u64, u64, u64, u64);
type Big128 = (Big32, Big32, Big32, Big32);
type Big512 = (Big128, Big128, Big128, Big128);
type Big2560 = (Big512, Big512, Big512, Big512, Big512);

#[derive(IntoHeap)]
struct Big<'h> {
    bits: Big2560,
    next: Option<BigRef<'h>>,
}

#[test]
fn size_big() {
    cell_gc::with_heap(|hs| {
        hs.set_page_limit::<Big>(Some(1));

        let n = cell_gc::page_capacity::<Big>();
        assert_eq!(n, 1); // see comment in size_medium.rs

        let a = (5, 6, 7, 8);
        let b = (a, a, a, a);
        let c = (b, b, b, b);
        let d = (c, c, c, c, c);
        let result = hs.alloc(Big {
            bits: d,
            next: None,
        });
        assert_eq!(result.bits(), d);
        assert_eq!(result.next(), None);

        assert_eq!(
            hs.try_alloc(Big {
                bits: d,
                next: None,
            }),
            None
        );
    });
}
