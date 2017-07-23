use cell_gc::{GcHeapSession, GcLeaf};
use cell_gc::collections::VecRef;
use compile;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};
use std::collections::HashSet;
use vm::{EnvironmentRef, Trampoline};

#[derive(Debug, IntoHeap)]
pub struct Pair<'h> {
    pub car: Value<'h>,
    pub cdr: Value<'h>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, IntoHeap)]
pub enum Value<'h> {
    Nil,
    Bool(bool),
    Int(i32),
    Char(char),
    Symbol(GcLeaf<InternedString>),
    ImmString(GcLeaf<InternedString>),
    Lambda(PairRef<'h>),
    Code(compile::CodeRef<'h>),
    Builtin(GcLeaf<BuiltinFnPtr>),
    Cons(PairRef<'h>),
    Vector(VecRef<'h, Value<'h>>),
    Environment(EnvironmentRef<'h>),
}

pub use self::Value::*;

pub type BuiltinFn =
    for<'b> fn(&mut GcHeapSession<'b>, Vec<Value<'b>>)
        -> Result<Trampoline<'b>, String>;

#[derive(Copy)]
pub struct BuiltinFnPtr(pub BuiltinFn);

// This can't be #[derive]d because function pointers aren't Clone.
// But they are Copy. A very weird thing about Rust.
impl Clone for BuiltinFnPtr {
    fn clone(&self) -> BuiltinFnPtr {
        BuiltinFnPtr(self.0)
    }
}

impl Hash for BuiltinFnPtr {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl PartialEq for BuiltinFnPtr {
    fn eq(&self, other: &BuiltinFnPtr) -> bool {
        self.0 as usize == other.0 as usize
    }
}

impl Eq for BuiltinFnPtr {}

impl fmt::Debug for BuiltinFnPtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BuiltinFn({:p})", self.0 as usize as *mut ())?;
        Ok(())
    }
}

/// A structure to `Display` a value.
pub struct PrintValue<'h> {
    value: Value<'h>,
    // Prevent inifinite recursion from cycles created with `set-car!` and the
    // like.
    seen: RefCell<HashSet<Value<'h>>>,
}

impl<'h> fmt::Display for PrintValue<'h> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.value {
            Cons(_) | Vector(_) => {
                if self.seen.borrow().contains(&self.value) {
                    // TODO: this should do the `#1#` style printing thing. Or maybe the
                    // standard has something to say about cyclic printing...
                    return write!(f, "<cycle>");
                }
                self.seen.borrow_mut().insert(self.value.clone());
            }
            _ => {}
        }

        match self.value {
            Nil => write!(f, "()"),
            Bool(true) => write!(f, "#t"),
            Bool(false) => write!(f, "#f"),
            Char(c) => write!(f, "#\\{}", c),
            Int(n) => write!(f, "{}", n),
            Symbol(ref s) => write!(f, "{}", s.as_str()),
            ImmString(ref s) => write!(f, "{:?}", s.as_str()),
            Lambda(_) => write!(f, "#lambda"),
            Code(_) => write!(f, "#code"),
            Builtin(_) => write!(f, "#builtin"),
            Cons(ref p) => {
                write!(f, "(")?;
                write_pair(f, p.clone(), &mut *self.seen.borrow_mut())?;
                write!(f, ")")
            }
            Vector(ref v) => {
                write!(f, "#(")?;
                for i in 0..v.len() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    let print_vi = PrintValue {
                        value: v.get(i),
                        seen: RefCell::new(mem::replace(&mut *self.seen.borrow_mut(), Default::default())),
                    };
                    write!(f, "{}", print_vi)?;
                    mem::swap(&mut *self.seen.borrow_mut(), &mut *print_vi.seen.borrow_mut());
                }
                write!(f, ")")
            }
            Environment(_) => write!(f, "#environment"),
        }
    }
}

fn write_pair<'h>(f: &mut fmt::Formatter, pair: PairRef<'h>, seen: &mut HashSet<Value<'h>>) -> fmt::Result {
    let print_car = PrintValue {
        value: pair.car(),
        seen: RefCell::new(mem::replace(seen, Default::default())),
    };
    write!(f, "{}", print_car)?;
    mem::swap(seen, &mut *print_car.seen.borrow_mut());

    match pair.cdr() {
        Nil => Ok(()),
        Cons(p) => {
            write!(f, " ")?;
            write_pair(f, p, seen)
        }
        otherwise => {
            write!(f, " . ")?;
            let print_cdr = PrintValue {
                value: otherwise,
                seen: RefCell::new(mem::replace(seen, Default::default())),
            };
            write!(f, "{}", print_cdr)?;
            mem::swap(seen, &mut *print_cdr.seen.borrow_mut());
            Ok(())
        }
    }
}

macro_rules! pattern_predicate {
    { $method:ident, $pat:pat } => {
        pub fn $method(&self) -> bool {
            match *self {
                $pat => true,
                _ => false
            }
        }
    }
}

impl<'h> Value<'h> {
    pattern_predicate!(is_nil, Nil);
    pattern_predicate!(is_boolean, Bool(_));

    /// True unless this value is `#f`. Conditional expressions (`if`, `cond`,
    /// etc.) should use this to check whether a value is a "true value".
    pub fn to_bool(&self) -> bool {
        match *self {
            Bool(false) => false,
            _ => true,
        }
    }

    pattern_predicate!(is_char, Char(_));

    pub fn as_char(self, error_msg: &str) -> Result<char, String> {
        match self {
            Char(c) => Ok(c),
            _ => Err(format!("{}: character required", error_msg))
        }
    }

    pattern_predicate!(is_number, Int(_));

    pub fn as_int(self, error_msg: &str) -> Result<i32, String> {
        match self {
            Int(i) => Ok(i),
            _ => Err(format!("{}: number required", error_msg)),
        }
    }

    pub fn as_index(self, error_msg: &str) -> Result<usize, String> {
        match self {
            Int(i) => {
                if i >= 0 {
                    Ok(i as usize)
                } else {
                    Err(format!("{}: negative vector index", error_msg))
                }
            }
            _ => Err(format!("{}: vector index required", error_msg)),
        }
    }

    pattern_predicate!(is_pair, Cons(_));

    pub fn as_pair(self, msg: &'static str) -> Result<(Value<'h>, Value<'h>), String> {
        match self {
            Cons(r) => Ok((r.car(), r.cdr())),
            _ => Err(msg.to_string()),
        }
    }

    pattern_predicate!(is_vector, Vector(_));

    pub fn as_vector(self, error_msg: &str) -> Result<VecRef<'h, Value<'h>>, String> {
        match self {
            Vector(v) => Ok(v),
            _ => Err(format!("{}: vector expected", error_msg)),
        }
    }

    pattern_predicate!(is_symbol, Symbol(_));

    pub fn as_symbol(self, error_msg: &str) -> Result<InternedString, String> {
        match self {
            Symbol(s) => Ok(s.unwrap()),
            _ => Err(error_msg.to_string()),
        }
    }

    pattern_predicate!(is_string, ImmString(_));

    pub fn as_string(self, error_msg: &str) -> Result<InternedString, String> {
        match self {
            ImmString(s) => Ok(s.unwrap()),
            _ => Err(error_msg.to_string()),
        }
    }

    pub fn is_procedure(&self) -> bool {
        match *self {
            Lambda(_) => true,
            Builtin(_) => true,
            _ => false,
        }
    }

    pub fn as_environment(self, error_msg: &str) -> Result<EnvironmentRef<'h>, String> {
        match self {
            Environment(env) => Ok(env),
            _ => Err(error_msg.to_string())
        }
    }

    pub fn print(self) -> PrintValue<'h> {
        PrintValue {
            value: self,
            seen: Default::default(),
        }
    }
}

impl<'h> Iterator for Value<'h> {
    type Item = Result<Value<'h>, String>;

    fn next(&mut self) -> Option<Self::Item> {
        let (car, cdr) = match *self {
            Nil => return None,
            Cons(ref pair) => (pair.car(), pair.cdr()),
            _ => return Some(Err("improper list".into())),
        };
        *self = cdr;
        Some(Ok(car))
    }
}


#[derive(Clone, Debug)]
pub struct InternedString(Arc<String>);

// Note: If we ever impl Hash for InternedString, it will be better to use a
// custom pointer-based implementation than to use derive(Hash), which would
// hash the contents of the string.
impl PartialEq for InternedString {
    fn eq(&self, other: &InternedString) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for InternedString {}

impl Hash for InternedString {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr: &str = &**self.0;
        let ptr = ptr as *const str;
        let ptr = ptr as *const ();
        let ptr = ptr as usize;
        ptr.hash(state);
    }
}

lazy_static! {
    static ref STRINGS: Mutex<HashSet<InternedStringByValue>> = Mutex::new(HashSet::new());
}

static GENSYM_COUNT: AtomicUsize = ATOMIC_USIZE_INIT;

#[derive(Eq, Hash, PartialEq)]
struct InternedStringByValue(Arc<String>);

impl Borrow<str> for InternedStringByValue {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl InternedString {
    /// Return an InternedString that is not interned.
    pub fn gensym() -> InternedString {
        let n = GENSYM_COUNT.fetch_add(1, Ordering::SeqCst);
        InternedString(Arc::new(format!("#<gensym{}>", n)))
    }

    pub fn get<T: AsRef<str> + Into<String>>(t: T) -> InternedString {
        let mut guard = STRINGS.lock().unwrap();
        if let Some(x) = guard.get(t.as_ref()) {
            return InternedString(x.0.clone());
        }
        let s = Arc::new(t.into());
        guard.insert(InternedStringByValue(s.clone()));
        InternedString(s)
    }

    pub fn really_intern(self) -> InternedString {
        if !self.0.starts_with("#<gensym") {
            return self;
        }
        let mut guard = STRINGS.lock().unwrap();
        {
            let s: &str = &self.0;
            match guard.get(s) {
                Some(interned) => return InternedString(interned.0.clone()),
                None => {}
            }
        }

        // Don't cause other references to this string to become interned!
        let new_arc = {
            let s: &str = &self.0;
            Arc::new(s.to_string())
        };
        guard.insert(InternedStringByValue(new_arc));
        self
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn is_interned(&self) -> bool {
        let guard = STRINGS.lock().unwrap();
        let s: &str = &self.0;
        match guard.get(s) {
            None => false,
            Some(interned) => Arc::ptr_eq(&interned.0, &self.0)
        }
    }

    pub fn is_gensym(&self) -> bool { !self.is_interned() }
}

impl Borrow<str> for InternedString {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}
