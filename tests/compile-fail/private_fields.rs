// Derived accessors that access private fields are private.

extern crate cell_gc;
#[macro_use] extern crate cell_gc_derive;

mod flowers {
    use cell_gc::GcHeapSession;

    #[derive(IntoHeap)]
    pub struct Clade<'h> {
        supertype: Option<CladeRef<'h>>,
        pub name: String,
    }

    impl<'h> Clade<'h> {
        pub fn new(hs: &GcHeapSession<'h>,
                   supertype: Option<CladeRef<'h>>,
                   name: String)
            -> CladeRef<'h>
        {
            hs.alloc(Clade {
                supertype: supertype,
                name: name
            })
        }
    }
}

fn main() {
    cell_gc::with_heap(|hs| {
        let clade = flowers::Clade::new(hs, None, "Angiospermae".to_string());
        let parent = clade.supertype();  // error
        //~^ ERROR: method `supertype` is private
        clade.set_supertype(None);  // error
        //~^ ERROR: method `set_supertype` is private
        let name = clade.name(); // ok
        clade.set_name("Magnoliophyta".to_string());  // ok

        assert_eq!(parent, None);
        assert_eq!(name, "Angiospermae");
    });
}
