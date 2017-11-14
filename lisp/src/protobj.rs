/// A simple prototypal object system.

use cell_gc::collections::VecRef;
use cell_gc::GcHeapSession;
use cell_gc::GcLeaf;
use value::Value;
use value::InternedString;

#[derive(Debug, IntoHeap)]
pub struct Shype<'h> {
    parent: Option<ShypeRef<'h>>,
    first_child: Option<ShypeRef<'h>>,
    pub next_sibling: Option<ShypeRef<'h>>,
    variant: ShypeVariant<'h>
}

#[derive(Debug, IntoHeap, PartialEq)]
pub enum ShypeVariant<'h> {
    Root,
    SetPrototype(ObjectRef<'h>),
    BecomePrototype(ShypeRef<'h>),
    AddProperty(GcLeaf<InternedString>, GcLeaf<PropDescr>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropDescr {
    Slot(u32)
    // Add Accessor entry.
}

impl<'h> Shype<'h> {
    pub fn new_root() -> Shype<'h> {
        Shype {
            parent: None,
            first_child: None,
            next_sibling: None,
            variant: ShypeVariant::Root
        }
    }

    fn new_child(hs: &mut GcHeapSession<'h>, parent: ShypeRef<'h>, variant: ShypeVariant<'h>)
        -> ShypeRef<'h>
    {
        let shype = hs.alloc(Shype {
            parent: Some(parent.clone()),
            first_child: None,
            next_sibling: parent.first_child(),
            variant: variant,
        });

        parent.set_first_child(Some(shype.clone()));
        shype
    }

    fn new_add_prop(
        hs: &mut GcHeapSession<'h>,
        parent: ShypeRef<'h>,
        name: &InternedString,
        slotno: u32,
    ) -> ShypeRef<'h> {
        Shype::new_child(hs, parent, ShypeVariant::AddProperty(GcLeaf::new(name.clone()),
                                                               GcLeaf::new(PropDescr::Slot(slotno))))
    }

    fn new_set_proto(
        hs: &mut GcHeapSession<'h>,
        parent: ShypeRef<'h>,
        proto: ObjectRef<'h>,
    ) -> ShypeRef<'h> {
        Shype::new_child(hs, parent, ShypeVariant::SetPrototype(proto))
    }

    fn new_become_proto(
        hs: &mut GcHeapSession<'h>,
        parent: ShypeRef<'h>,
        target_shype: ShypeRef<'h>,
    ) -> ShypeRef<'h> {
        Shype::new_child(hs, parent, ShypeVariant::BecomePrototype(target_shype))
    }
}

impl<'h> ShypeRef<'h> {
    pub fn is_root(&self) -> bool {
        self.variant() == ShypeVariant::Root
    }

    pub fn get_parent(&self) -> Option<ShypeRef<'h>> {
        self.parent().clone()
    }
}

pub struct ShypeParentIter<'h> {
    mb_shype: Option<ShypeRef<'h>>
}
impl<'h> Iterator for ShypeParentIter<'h> {
    type Item = ShypeRef<'h>;
    fn next(&mut self) -> Option<ShypeRef<'h>> {
        let ret = self.mb_shype.clone();
        if let Some(ref sr) = ret {
            self.mb_shype = sr.parent().clone();
        }
        ret
    }
}

pub struct ShypeNextSiblingIter<'h> {
    mb_shype: Option<ShypeRef<'h>>
}

impl<'h> Iterator for ShypeNextSiblingIter<'h> {
    type Item = ShypeRef<'h>;

    fn next(&mut self) -> Option<ShypeRef<'h>> {
        let ret = self.mb_shype.clone();
        if let Some(ref sr) = ret {
            self.mb_shype = sr.next_sibling().clone();
        }
        ret
    }
}

#[derive(Debug, IntoHeap)]
pub struct Object<'h> {
    pub shype: ShypeRef<'h>,
    prop_slots: VecRef<'h, Value<'h>>
}

impl<'h> Object<'h> {
    pub fn new(shype: ShypeRef<'h>, prop_slots: VecRef<'h, Value<'h>>) -> Object<'h> {
        // A new object always has a root shype
        assert!(shype.variant() == ShypeVariant::Root);
        assert!(prop_slots.len() == 0);
        Object {
            shype: shype,
            prop_slots: prop_slots
        }
    }
}

impl<'h> ObjectRef<'h> {
    pub fn allocate(hs: &mut GcHeapSession<'h>, shype: ShypeRef<'h>) -> ObjectRef<'h> {
        assert!(shype.variant() == ShypeVariant::Root);
        let vec = hs.alloc(Vec::new());
        hs.alloc(Object::new(shype, vec))
    }

    pub fn get_slot(&self, slotno: u32) -> Value<'h> {
        assert!((slotno as usize) < self.prop_slots().len());
        return self.prop_slots().get(slotno as usize).clone();
    }

    pub fn set_slot(&self, slotno: u32, val: Value<'h>) {
        assert!((slotno as usize) < self.prop_slots().len());
        self.prop_slots().set(slotno as usize, val);
    }

    pub fn num_slots(&self) -> u32 {
        assert!(self.prop_slots().len() <= u32::max_value() as usize);
        self.prop_slots().len() as u32
    }

    pub fn next_slotno(&self) -> u32 {
        self.num_slots()
    }

    pub fn add_slot(&self, val: Value<'h>) -> u32 {
        self.prop_slots().push(val);
        self.num_slots() - 1
    }
}

pub struct ObjectProtoIter<'h> {
    mb_object: Option<ObjectRef<'h>>
}

impl<'h> Iterator for ObjectProtoIter<'h> {
    type Item = ObjectRef<'h>;
    fn next(&mut self) -> Option<ObjectRef<'h>> {
        let ret = self.mb_object.clone();
        if let Some(ref obj) = ret {
            self.mb_object = obj.get_prototype();
        }
        ret
    }
}

impl<'h> ShypeRef<'h> {
    pub fn root_path_iter(&self) -> ShypeParentIter<'h> {
        ShypeParentIter { mb_shype: Some(self.clone()) }
    }

    pub fn children_iter(&self) -> ShypeNextSiblingIter<'h> {
        ShypeNextSiblingIter { mb_shype: self.first_child() }
    }

    pub fn each_addprop<T, I, F>(iter: I, mut f: F) -> Option<T>
        where I: Iterator<Item=ShypeRef<'h>>,
              F: FnMut(ShypeRef<'h>, &InternedString, &PropDescr) -> Option<T>
    {
        for shype in iter {
            if let ShypeVariant::AddProperty(ref name, ref descr) = shype.variant() {
                if let Some(result) = f(shype.clone(), name, descr) {
                    return Some(result);
                }
            }
        }
        None
    }

    fn select_named_addprop(name: &InternedString,
                            shype: ShypeRef<'h>,
                            prop_name: &InternedString,
                            descr: &PropDescr)
        -> Option<(ShypeRef<'h>, PropDescr)>
    {
        if prop_name == name {
            Some((shype, descr.clone()))
        } else {
            None
        }
    }

    fn lookup_root_path_addprop(&self, name: &InternedString)
        -> Option<(ShypeRef<'h>, PropDescr)>
    {
        Self::each_addprop(self.root_path_iter(), |shype, prop_name, descr| {
            Self::select_named_addprop(name, shype, prop_name, descr)
        })
    }

    fn lookup_child_addprop(&self, name: &InternedString)
        -> Option<(ShypeRef<'h>, PropDescr)>
    {
        Self::each_addprop(self.children_iter(), |shype, prop_name, descr| {
            Self::select_named_addprop(name, shype, prop_name, descr)
        })
    }

    pub fn new_object(&self, mb_proto: Option<ObjectRef<'h>>, hs: &mut GcHeapSession<'h>)
         -> ObjectRef<'h>
    {
        assert!(self.variant() == ShypeVariant::Root);

        // Create a new object with this shype.
        let obj = ObjectRef::allocate(hs, self.clone());

        // Set the prototype of this object to proto.
        if let Some(proto) = mb_proto {
            obj.set_prototype(proto, hs);
        }

        obj
    }

    pub fn get_prototype(&self) -> Option<ObjectRef<'h>> {
        for anc_shype in self.root_path_iter() {
            // Check for setPrototype 
            if let ShypeVariant::SetPrototype(ref proto) = anc_shype.variant() {
                return Some(proto.clone());
            }
        }

        None
    }

    pub fn set_prototype(&self, proto: ObjectRef<'h>, hs: &mut GcHeapSession<'h>)
        -> (ShypeRef<'h>, bool)
    {
        // First, check to see if the current proto is already the right one.
        for anc_shype in self.root_path_iter() {
            if let ShypeVariant::SetPrototype(ref pr) = anc_shype.variant() {
                if pr == &proto {
                    return (anc_shype, false);
                }
                break;
            }
        }

        // Check to see if a SetPrototype(proto) exists as a child shype.
        for ch_shype in self.children_iter() {
            if let ShypeVariant::SetPrototype(ref pr) = ch_shype.variant() {
                if pr == &proto {
                    return (ch_shype, true);
                }
                break;
            }
        }

        // Create a SetPrototype(proto) and add it as a child shype.
        (Shype::new_set_proto(hs, self.clone(), proto), true)
    }

    pub fn get_own_property(&self, name: &InternedString) -> Option<(ShypeRef<'h>, u32)> {
        // Look up to see if a shype exists for the property.
        if let Some((shype, descr)) = self.lookup_root_path_addprop(name) {
            match descr {
                PropDescr::Slot(slot) => { return Some((shype, slot)); }
            }
        }

        None
    }

    /** Does the shype lookup to sets the property `name` on object `obj` to `value`.
     * If the property is already defined on the object, that shype and the slot
     * number is returned.  If not, a new child shype is found or created for the
     * property and returned.
     *
     * Returns `(shype, slot, add)` where `shype` is the shype describing
     * the property, `slot` is the slot the value should be stored to, and
     * `add` indicates if the slot is to be added to the object (instead
     * of using an existing slot).
     */
    pub fn set_property(&self, obj: ObjectRef<'h>, name: &InternedString,
                                   hs: &mut GcHeapSession<'h>)
        -> (ShypeRef<'h>, u32, bool)
    {
        assert!(obj.shype() == *self);

        // Look up to see if a shype exists for the property.
        if let Some((shype, descr)) = self.lookup_root_path_addprop(name) {
            match descr {
                PropDescr::Slot(slot) => { return (shype, slot, false); }
            }
        }

        // Existing property not found, add it.

        // Check if a child property shype already exists for the to-be-added
        // property.
        if let Some((shype, descr)) = self.lookup_child_addprop(name) {
            match descr {
                PropDescr::Slot(slot) => {
                    assert!(slot == obj.num_slots());
                    return (shype, slot, true);
                }
            }
        }

        // Otherwise, create a new property shype as a child.
        let slot = obj.num_slots();
        (Shype::new_add_prop(hs, self.clone(), name, slot), slot, true)
    }

    pub fn own_property_names(&self) -> Vec<Value<'h>> {
        let mut result = Vec::new();
        for anc_shype in self.root_path_iter() {
            if let ShypeVariant::AddProperty(ref name, _) = anc_shype.variant() {
                result.push(Value::ImmString(name.clone()));
            }
        }
        result
    }

    pub fn has_ancestor_shype(&self, sh: ShypeRef<'h>) -> bool {
        // Look up to see if a shype exists for the property.
        for anc_sh in self.root_path_iter() {
            if anc_sh == sh { return true; }
        }
        false
    }

    pub fn has_own_property(&self, name: &InternedString) -> bool {
        // Look up to see if a shype exists for the property.
        if let Some((_, descr)) = self.lookup_root_path_addprop(name) {
            match descr {
                PropDescr::Slot(_) => { return true; }
            }
        }

        false
    }

    /** Return a shype that is either this shype or a descendant shype that models
     * a prototype-object.
     */
    pub fn become_prototype_of(&self, target_shype: ShypeRef<'h>, hs: &mut GcHeapSession<'h>)
        -> (ShypeRef<'h>, bool)
    {
        // Find any existing BecomeProto.
        let mut found : Option<ShypeRef<'h>> = None;
        for sh in self.root_path_iter() {
            if let ShypeVariant::BecomePrototype(target_sh) = sh.variant() {
                // Found the previous BecomeProto.  If it matches, return it, otherwise
                // return nothing.
                if &target_shype == &target_sh {
                    found = Some(sh);
                }
                break;
            }
        }

        if let Some(found_sh) = found {
            return (found_sh, false);
        }

        // Either this shype has no BecomePrototype in its parent chain, or the last
        // BecomePrototype is for a different shype.

        // Check child shypes for something matching.
        found = None;
        for sh in self.children_iter() {
            if let ShypeVariant::BecomePrototype(target_sh) = sh.variant() {
                if &target_shype == &target_sh {
                    found = Some(sh);
                }
                break;
            }
        }

        if let Some(found_sh) = found {
            return (found_sh, true);
        }

        // Otherwise, add a child BecomePrototype shype.
        (Shype::new_become_proto(hs, self.clone(), target_shype), true)
    }
}

impl<'h> ObjectRef<'h> {
    pub fn proto_chain_iter(&self) -> ObjectProtoIter<'h> {
        ObjectProtoIter { mb_object: Some(self.clone()) }
    }

    pub fn get_prototype(&self) -> Option<ObjectRef<'h>> {
        self.shype().get_prototype()
    }

    pub fn get_property(&self, name: &InternedString) -> Value<'h> {
        for obj in self.proto_chain_iter() {
            if let Some((_, slot)) = obj.shype().get_own_property(name) {
                return obj.get_slot(slot);
            }
        }

        Value::Bool(false)
    }

    pub fn set_property(&self, name: &InternedString, value: Value<'h>, hs: &mut GcHeapSession<'h>)
        -> ShypeRef<'h>
    {
        let old_shype = self.shype();
        let (shype, slot, add) = old_shype.set_property(self.clone(), name, hs);
        assert!(slot <= self.num_slots());
        if add {
            assert!(shype.parent() == Some(self.shype()));
            assert!(slot == self.num_slots());
            let added_slot = self.add_slot(value);
            assert!(added_slot == slot);
            self.set_shype(shype.clone());
        } else {
            assert!(old_shype.has_ancestor_shype(shype.clone()));
            assert!(slot < self.num_slots());
            self.set_slot(slot, value);
        }
        shype
    }

    pub fn become_prototype_of(&self, target_shype: ShypeRef<'h>, hs: &mut GcHeapSession<'h>)
        -> ShypeRef<'h>
    {
        let old_shype = self.shype();
        let (shype, set) = old_shype.become_prototype_of(target_shype, hs);
        if set {
            assert!(shype.parent() == Some(self.shype()));
            self.set_shype(shype.clone());
        } else {
            assert!(old_shype.has_ancestor_shype(shype.clone()));
        }
        shype
    }

    pub fn set_prototype(&self, proto: ObjectRef<'h>, hs: &mut GcHeapSession<'h>)
        -> ShypeRef<'h>
    {
        // Get the shype that the target object needs to have.
        let old_shype = self.shype();
        let (setproto_shype, set_target) = old_shype.set_prototype(proto.clone(), hs);
        let target_shype = if set_target { setproto_shype.clone() } else { old_shype };

        // Get the shype that the proto object needs to have.
        proto.become_prototype_of(target_shype.clone(), hs);

        if set_target {
            assert!(&setproto_shype == &target_shype);
            self.set_shype(setproto_shype.clone());
        }
        setproto_shype
    }

    pub fn has_own_property(&self, name: &InternedString) -> bool {
        self.shype().has_own_property(name)
    }

    pub fn own_property_names(&self) -> Vec<Value<'h>> {
        self.shype().own_property_names()
    }
}
