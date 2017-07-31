//! Test that objects reachable from a root's `.head` or `.tail` are not
//! collected.

extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;
mod aux;
use aux::pairs::*;

#[test]
fn reachable_objects_not_collected() {
    cell_gc::with_heap(|hs| {
        let obj1 = alloc_null_pair(hs);
        let (p2, p3, p4, p5);
        {
            let obj2 = alloc_null_pair(hs);
            p2 = obj2.as_mut_ptr();
            obj1.set_head(Value::Pair(obj2.clone()));
            let obj3 = alloc_null_pair(hs);
            p3 = obj3.as_mut_ptr();
            obj1.set_tail(Value::Pair(obj3));
            let obj4 = alloc_null_pair(hs);
            p4 = obj4.as_mut_ptr();
            obj2.set_head(Value::Pair(obj4));
            let obj5 = alloc_null_pair(hs);
            p5 = obj5.as_mut_ptr();
            obj2.set_tail(Value::Pair(obj5));
        }

        hs.force_gc();

        let h = match obj1.head() {
            Value::Pair(p) => p,
            _ => panic!("expected pair in obj1.head"),
        };
        assert_eq!(h.as_mut_ptr(), p2);

        let t = match obj1.tail() {
            Value::Pair(p) => p,
            _ => panic!("expected pair in obj1.tail"),
        };
        assert_eq!(t.as_mut_ptr(), p3);

        let hh = match h.head() {
            Value::Pair(p) => p,
            _ => panic!("expected pair in obj1.head.head"),
        };
        assert_eq!(hh.as_mut_ptr(), p4);

        let ht = match h.tail() {
            Value::Pair(p) => p,
            _ => panic!("expected pair in obj1.head.tail"),
        };
        assert_eq!(ht.as_mut_ptr(), p5);
    });
}
