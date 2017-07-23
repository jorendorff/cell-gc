use cell_gc::{GcHeapSession, GcLeaf};
use cell_gc::collections::VecRef;
use compile;
use std::borrow::Borrow;
use std::fmt;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};
use std::collections::HashSet;
use vm::{EnvironmentRef, Trampoline};

#[derive(Debug, IntoHeap)]
pub struct Pair<'h> {
    pub car: Value<'h>,
    pub cdr: Value<'h>,
}

#[derive(Clone, Debug, PartialEq, IntoHeap)]
pub enum Value<'h> {
    Unspecified,
    Nil,
    Bool(bool),
    Int(i32),
    Char(char),
    Symbol(GcLeaf<InternedString>),
    StringObj(GcLeaf<NonInternedStringObject>),
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

pub struct BuiltinFnPtr(pub BuiltinFn);

// This can't be #[derive]d because function pointers aren't Clone.
// But they are Copy. A very weird thing about Rust.
impl Clone for BuiltinFnPtr {
    fn clone(&self) -> BuiltinFnPtr {
        BuiltinFnPtr(self.0)
    }
}

impl PartialEq for BuiltinFnPtr {
    fn eq(&self, other: &BuiltinFnPtr) -> bool {
        self.0 as usize == other.0 as usize
    }
}

impl fmt::Debug for BuiltinFnPtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BuiltinFn({:p})", self.0 as usize as *mut ())?;
        Ok(())
    }
}

impl<'h> fmt::Display for Value<'h> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Note that this will need to add a set of already-printed pairs if we add
        // `set-car!` and/or `set-cdr!` and introduce the possibility of cycles.
        match *self {
            Unspecified => write!(f, "#<unspecified>"),
            Nil => write!(f, "()"),
            Bool(true) => write!(f, "#t"),
            Bool(false) => write!(f, "#f"),
            Char(c) => write!(f, "#\\{}", c),
            Int(n) => write!(f, "{}", n),
            Symbol(ref s) => write!(f, "{}", s.as_str()),
            StringObj(ref s) => write!(f, "{:?}", s as &str),
            ImmString(ref s) => write!(f, "{:?}", s.as_str()),
            Lambda(_) => write!(f, "#lambda"),
            Code(_) => write!(f, "#code"),
            Builtin(_) => write!(f, "#builtin"),
            Cons(ref p) => {
                write!(f, "(")?;
                write_pair(f, p.clone())?;
                write!(f, ")")
            }
            Vector(ref v) => {
                write!(f, "#(")?;
                for i in 0..v.len() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", v.get(i))?;
                }
                write!(f, ")")
            }
            Environment(_) => write!(f, "#environment"),
        }
    }
}

fn write_pair<'h>(f: &mut fmt::Formatter, pair: PairRef<'h>) -> fmt::Result {
    write!(f, "{}", pair.car())?;
    match pair.cdr() {
        Nil => Ok(()),
        Cons(p) => {
            write!(f, " ")?;
            write_pair(f, p)
        }
        otherwise => {
            write!(f, " . ")?;
            write!(f, "{}", otherwise)
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
    pattern_predicate!(is_unspecified, Unspecified);
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
            _ => Err(format!("{}: symbol required", error_msg)),
        }
    }

    pub fn is_string(&self) -> bool {
        match *self {
            ImmString(_) | StringObj(_) => true,
            _ => false
        }
    }

    pub fn as_string(self, error_msg: &str) -> Result<Arc<String>, String> {
        match self {
            ImmString(s) => Ok(s.unwrap().0),
            StringObj(s) => Ok(s.unwrap().0),
            _ => Err(format!("{}: string required", error_msg)),
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
pub struct NonInternedStringObject(pub Arc<String>);

impl PartialEq for NonInternedStringObject {
    fn eq(&self, other: &NonInternedStringObject) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for NonInternedStringObject {}

impl ::std::ops::Deref for NonInternedStringObject {
    type Target = Arc<String>;
    fn deref(&self) -> &Arc<String> { &self.0 }
}

#[derive(Clone, Debug)]
pub struct InternedString(Arc<String>);

// Note: If we ever impl Hash for InternedString, it will be better to use a
// custom pointer-based implementation than to use derive(Hash), which would
// hash the contents of the string. (Note however that we currently allow
// InternedStrings that are not actually interned. Kind of a disaster.)
impl PartialEq for InternedString {
    fn eq(&self, other: &InternedString) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for InternedString {}

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

    pub fn intern_arc(s: Arc<String>) -> InternedString {
        let mut guard = STRINGS.lock().unwrap();
        if let Some(x) = guard.get(&s as &str) {
            return InternedString(x.0.clone());
        }
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

    pub fn as_string(&self) -> &String {
        &self.0
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
