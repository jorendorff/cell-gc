//! The GC can work with objects that take up most of a page.

#[macro_use] extern crate toy_gc;

pub type Big32 = (u64, u64, u64, u64);
pub type Big128 = (Big32, Big32, Big32, Big32);
pub type Big512 = (Big128, Big128, Big128, Big128);
pub type Big2560 = (Big512, Big512, Big512, Big512, Big512);

gc_ref_type! {
    pub struct Big / BigRef / BigStorage / BigRefStorage <'a> {
        bits / set_bits: Big2560,
        next / set_next: Option<BigRef<'a>>
    }
}

fn main () {
    toy_gc::with_heap(|heap| {
        let n = toy_gc::page_capacity::<Big>();
        assert_eq!(n, 1);  // see comment in size_medium.rs

        let a = (5, 6, 7, 8);
        let b = (a, a, a, a);
        let c = (b, b, b, b);
        let d = (c, c, c, c, c);
        let result = heap.alloc(Big {
            bits: d,
            next: None
        });
        assert_eq!(result.bits(), d);
        assert_eq!(result.next(), None);

        assert_eq!(heap.try_alloc(Big {bits: d, next: None}), None);
    });
}
