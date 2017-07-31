extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;

use cell_gc::GcHeap;
use cell_gc::collections::VecRef;

#[test]
fn vec_ref() {
    #[derive(IntoHeap)]
    struct Car<'h> {
        wheels: VecRef<'h, String>
    }

    let mut heap = GcHeap::new();
    heap.enter(|hs| {
        let wheels = vec![
            "lf".to_string(),
            "rf".to_string(),
            "lr".to_string(),
            "rr".to_string(),
        ];
        let wheels_gc = hs.alloc(wheels);
        let car = hs.alloc(Car {
            wheels: wheels_gc
        });
        hs.force_gc();
        hs.force_gc();
        assert_eq!(car.wheels().len(), 4);
        assert_eq!(car.wheels().get(3), "rr");
    });
}
