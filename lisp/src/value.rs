use cell_gc::{GcHeapSession, GcLeaf};
use cell_gc::collections::VecRef;
use compile;
use errors::Result;
use std::borrow::Borrow;
use std::collections::HashSet;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Mutex};
use std::sync::atomic::{ATOMIC_USIZE_INIT, AtomicUsize, Ordering};
use vm::{EnvironmentRef, Trampoline};

#[derive(Debug, IntoHeap)]
pub struct Pair<'h> {
    pub car: Value<'h>,
    pub cdr: Value<'h>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, IntoHeap)]
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

pub type BuiltinFn = for<'b> fn(&mut GcHeapSession<'b>, Vec<Value<'b>>)
    -> Result<Trampoline<'b>>;

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

impl<'h> fmt::Display for Value<'h> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.print(f, false, &mut HashSet::new())
    }
}

/// The `(display)` procedure prints values in a different style: strings and
/// characters are written verbatim. Use this wrapper type to get that
/// style of output.
pub struct DisplayValue<'h>(pub Value<'h>);

impl<'h> fmt::Display for DisplayValue<'h> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.print(f, true, &mut HashSet::new())
    }
}


fn write_pair<'h>(
    f: &mut fmt::Formatter,
    pair: PairRef<'h>,
    display: bool,
    seen: &mut HashSet<Value<'h>>,
) -> fmt::Result {
    pair.car().print(f, display, seen)?;

    match pair.cdr() {
        Nil => Ok(()),
        Cons(p) => {
            write!(f, " ")?;
            write_pair(f, p, display, seen)
        }
        otherwise => {
            write!(f, " . ")?;
            otherwise.print(f, display, seen)?;
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

    pub fn as_char(self, error_msg: &str) -> Result<char> {
        match self {
            Char(c) => Ok(c),
            _ => Err(format!("{}: character required", error_msg).into()),
        }
    }

    pattern_predicate!(is_number, Int(_));

    pub fn as_int(self, error_msg: &str) -> Result<i32> {
        match self {
            Int(i) => Ok(i),
            _ => Err(format!("{}: number required", error_msg).into()),
        }
    }

    pub fn as_index(self, error_msg: &str) -> Result<usize> {
        match self {
            Int(i) => {
                if i >= 0 {
                    Ok(i as usize)
                } else {
                    Err(format!("{}: negative vector index", error_msg).into())
                }
            }
            _ => Err(format!("{}: vector index required", error_msg).into()),
        }
    }

    pattern_predicate!(is_pair, Cons(_));

    pub fn as_pair_ref(self, error_msg: &str) -> Result<PairRef<'h>> {
        match self {
            Cons(r) => Ok(r),
            _ => Err(format!("{}: pair required", error_msg).into()),
        }
    }

    pub fn as_pair(self, error_msg: &str) -> Result<(Value<'h>, Value<'h>)> {
        match self {
            Cons(r) => Ok((r.car(), r.cdr())),
            _ => Err(format!("{}: pair required", error_msg).into()),
        }
    }

    pattern_predicate!(is_vector, Vector(_));

    pub fn as_vector(self, error_msg: &str) -> Result<VecRef<'h, Value<'h>>> {
        match self {
            Vector(v) => Ok(v),
            _ => Err(format!("{}: vector expected", error_msg).into()),
        }
    }

    pattern_predicate!(is_symbol, Symbol(_));

    pub fn as_symbol(self, error_msg: &str) -> Result<InternedString> {
        match self {
            Symbol(s) => Ok(s.unwrap()),
            _ => Err(format!("{}: symbol required", error_msg).into()),
        }
    }

    pub fn is_string(&self) -> bool {
        match *self {
            ImmString(_) | StringObj(_) => true,
            _ => false,
        }
    }

    pub fn as_string(self, error_msg: &str) -> Result<Arc<String>> {
        match self {
            ImmString(s) => Ok(s.unwrap().0),
            StringObj(s) => Ok(s.unwrap().0),
            _ => Err(format!("{}: string required", error_msg).into()),
        }
    }

    pub fn is_procedure(&self) -> bool {
        match *self {
            Lambda(_) => true,
            Builtin(_) => true,
            _ => false,
        }
    }

    pub fn as_environment(self, error_msg: &str) -> Result<EnvironmentRef<'h>> {
        match self {
            Environment(env) => Ok(env),
            _ => Err(format!("{}: environment required", error_msg).into()),
        }
    }

    fn print(&self, f: &mut fmt::Formatter, display: bool, seen: &mut HashSet<Value<'h>>)
        -> fmt::Result
    {
        match *self {
            Cons(_) | Vector(_) => {
                if seen.contains(self) {
                    // TODO: this should do the `#1#` style printing thing. Or maybe the
                    // standard has something to say about cyclic printing...
                    return write!(f, "<cycle>");
                }
                seen.insert(self.clone());
            }
            _ => {}
        }

        match *self {
            Unspecified => write!(f, "#<unspecified>"),
            Nil => write!(f, "()"),
            Bool(true) => write!(f, "#t"),
            Bool(false) => write!(f, "#f"),
            Char(c) =>
                if display {
                    write!(f, "{}", c)
                } else {
                    write!(f, "#\\{}", c)
                },
            Int(n) => write!(f, "{}", n),
            Symbol(ref s) => write!(f, "{}", s.as_str()),
            StringObj(ref s) =>
                if display {
                    write!(f, "{}", s.as_str())
                } else {
                    write!(f, "{:?}", s.as_str())
                },
            ImmString(ref s) =>
                if display {
                    write!(f, "{}", s.as_str())
                } else {
                    write!(f, "{:?}", s.as_str())
                },
            Lambda(_) => write!(f, "#lambda"),
            Code(_) => write!(f, "#code"),
            Builtin(_) => write!(f, "#builtin"),
            Cons(ref p) => {
                write!(f, "(")?;
                write_pair(f, p.clone(), display, seen)?;
                write!(f, ")")
            }
            Vector(ref v) => {
                write!(f, "#(")?;
                for i in 0..v.len() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    v.get(i).print(f, display, seen)?;
                }
                write!(f, ")")
            }
            Environment(_) => write!(f, "#environment"),
        }
    }
}

impl<'h> Iterator for Value<'h> {
    type Item = Result<Value<'h>>;

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

impl Hash for NonInternedStringObject {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr: *const String = &self.0 as &String;
        ptr.hash(state);
    }
}

impl ::std::ops::Deref for NonInternedStringObject {
    type Target = Arc<String>;
    fn deref(&self) -> &Arc<String> {
        &self.0
    }
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

    pub fn intern_arc(s: Arc<String>) -> InternedString {
        let mut guard = STRINGS.lock().unwrap();
        if let Some(x) = guard.get(&s as &str) {
            return InternedString(x.0.clone());
        }
        guard.insert(InternedStringByValue(s.clone()));
        InternedString(s)
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
            Some(interned) => Arc::ptr_eq(&interned.0, &self.0),
        }
    }

    pub fn is_gensym(&self) -> bool {
        !self.is_interned()
    }
}

impl Borrow<str> for InternedString {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}


/// Conversion traits //////////////////////////////////////////////////////////

pub trait ArgType<'h>: Sized {
    fn try_unpack(proc_name: &str, value: Value<'h>) -> Result<Self>;
}

impl<'h> ArgType<'h> for Value<'h> {
    fn try_unpack(_: &str, value: Value<'h>) -> Result<Value<'h>> {
        Ok(value)
    }
}

impl<'h> ArgType<'h> for char {
    fn try_unpack(proc_name: &str, value: Value<'h>) -> Result<char> {
        value.as_char(proc_name)
    }
}

impl<'h> ArgType<'h> for i32 {
    fn try_unpack(proc_name: &str, value: Value<'h>) -> Result<i32> {
        value.as_int(proc_name)
    }
}

impl<'h> ArgType<'h> for usize {
    fn try_unpack(proc_name: &str, value: Value<'h>) -> Result<usize> {
        value.as_index(proc_name)
    }
}

impl<'h> ArgType<'h> for Arc<String> {
    fn try_unpack(proc_name: &str, value: Value<'h>) -> Result<Arc<String>> {
        value.as_string(proc_name)
    }
}

impl<'h> ArgType<'h> for PairRef<'h> {
    fn try_unpack(proc_name: &str, value: Value<'h>) -> Result<PairRef<'h>> {
        value.as_pair_ref(proc_name)
    }
}

impl<'h> ArgType<'h> for VecRef<'h, Value<'h>> {
    fn try_unpack(proc_name: &str, value: Value<'h>) -> Result<VecRef<'h, Value<'h>>> {
        value.as_vector(proc_name)
    }
}

impl<'h> ArgType<'h> for EnvironmentRef<'h> {
    fn try_unpack(proc_name: &str, value: Value<'h>) -> Result<EnvironmentRef<'h>> {
        value.as_environment(proc_name)
    }
}


pub trait RetType<'h> {
    fn pack(self, hs: &mut GcHeapSession<'h>) -> Result<Trampoline<'h>>;
}

impl<'h> RetType<'h> for Value<'h> {
    fn pack(self, _hs: &mut GcHeapSession<'h>) -> Result<Trampoline<'h>> {
        Ok(Trampoline::Value(self))
    }
}

impl<'h, T: RetType<'h>> RetType<'h> for Result<T> {
    fn pack(self, hs: &mut GcHeapSession<'h>) -> Result<Trampoline<'h>> {
        self.and_then(|x| <T as RetType<'h>>::pack(x, hs))
    }
}

impl<'h> RetType<'h> for bool {
    fn pack(self, _hs: &mut GcHeapSession<'h>) -> Result<Trampoline<'h>> {
        Ok(Trampoline::Value(Value::Bool(self)))
    }
}

impl<'h> RetType<'h> for char {
    fn pack(self, _hs: &mut GcHeapSession<'h>) -> Result<Trampoline<'h>> {
        Ok(Trampoline::Value(Value::Char(self)))
    }
}

impl<'h> RetType<'h> for usize {
    fn pack(self, _hs: &mut GcHeapSession<'h>) -> Result<Trampoline<'h>> {
        if self > i32::max_value() as usize {
            return Err("string-length: integer overflow".into());
        }
        Ok(Trampoline::Value(Value::Int(self as i32)))
    }
}

impl<'h> RetType<'h> for String {
    fn pack(self, _hs: &mut GcHeapSession<'h>) -> Result<Trampoline<'h>> {
        Ok(Trampoline::Value(
            StringObj(GcLeaf::new(NonInternedStringObject(Arc::new(self)))),
        ))
    }
}

impl<'h> RetType<'h> for PairRef<'h> {
    fn pack(self, _hs: &mut GcHeapSession<'h>) -> Result<Trampoline<'h>> {
        Ok(Trampoline::Value(Value::Cons(self)))
    }
}

impl<'h> RetType<'h> for Vec<Value<'h>> {
    fn pack(self, hs: &mut GcHeapSession<'h>) -> Result<Trampoline<'h>> {
        Ok(Trampoline::Value(Value::Vector(hs.alloc(self))))
    }
}

impl<'h> RetType<'h> for () {
    fn pack(self, _hs: &mut GcHeapSession<'h>) -> Result<Trampoline<'h>> {
        Ok(Trampoline::Value(Value::Unspecified))
    }
}

impl<'h> RetType<'h> for Trampoline<'h> {
    fn pack(self, _hs: &mut GcHeapSession<'h>) -> Result<Trampoline<'h>> {
        Ok(self)
    }
}
