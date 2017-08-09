// Environments store variables (as name-value pairs).

use cell_gc::{GcHeapSession, GcLeaf};
use cell_gc::collections::VecRef;
use errors::Result;
use value::{InternedString, Pair, Value};
use vm;

#[derive(IntoHeap)]
pub struct StaticEnvironment<'h> {
    pub parent: Option<StaticEnvironmentRef<'h>>,
    pub names: VecRef<'h, GcLeaf<InternedString>>,
}

#[derive(Debug, IntoHeap)]
pub struct Environment<'h> {
    pub parent: Option<EnvironmentRef<'h>>,
    pub senv: StaticEnvironmentRef<'h>,
    values: VecRef<'h, Value<'h>>,
}


impl<'h> StaticEnvironmentRef<'h> {
    pub fn lookup(&self, name: &InternedString) -> Option<(usize, usize)> {
        let mut up_count = 0;
        let mut next = Some(self.clone());
        while let Some(senv) = next {
            let names = senv.names();
            for (index, s) in names.into_iter().enumerate() {
                if name == &*s {
                    return Some((up_count, index));
                }
            }
            next = senv.parent();
            up_count += 1;
        }
        None
    }

    pub fn up(self, up_count: usize) -> StaticEnvironmentRef<'h> {
        let mut senv = self;
        for _ in 0..up_count {
            senv = senv.parent().unwrap();
        }
        senv
    }

    pub fn get_name(&self, up_count: usize, index: usize) -> InternedString {
        self.clone().up(up_count).names().get(index).unwrap()
    }

    pub fn new_nested_environment(
        &self,
        hs: &mut GcHeapSession<'h>,
        names: Vec<GcLeaf<InternedString>>,
    ) -> StaticEnvironmentRef<'h> {
        let names = hs.alloc(names);
        hs.alloc(StaticEnvironment {
            parent: Some(self.clone()),
            names: names,
        })
    }
}

impl<'h> Environment<'h> {
    pub fn empty(hs: &mut GcHeapSession<'h>) -> EnvironmentRef<'h> {
        let senv = StaticEnvironment {
            parent: None,
            names: hs.alloc(vec![]),
        };
        let senv = hs.alloc(senv);
        let env = Environment {
            parent: None,
            senv: senv.clone(),
            values: hs.alloc(vec![]),
        };
        hs.alloc(env)
    }

    pub fn new(
        hs: &mut GcHeapSession<'h>,
        parent: Option<EnvironmentRef<'h>>,
        senv: StaticEnvironmentRef<'h>,
        values: VecRef<'h, Value<'h>>,
    ) -> EnvironmentRef<'h> {
        debug_assert_eq!(senv.names().len(), values.len());
        let env = hs.alloc(Environment {
            parent,
            senv,
            values,
        });
        env.debug_assert_consistent();
        env
    }
}

lazy_static! {
    static ref EXPANDER_SYMBOL: InternedString = InternedString::gensym();
}

impl<'h> EnvironmentRef<'h> {
    pub fn new_nested_environment(&self, hs: &mut GcHeapSession<'h>) -> EnvironmentRef<'h> {
        let xsenv = self.senv().new_nested_environment(hs, vec![]);
        let values = hs.alloc(vec![]);
        Environment::new(hs, Some(self.clone()), xsenv, values)
    }

    pub fn push(&self, key: InternedString, value: Value<'h>) {
        self.senv().names().push(GcLeaf::new(key));
        self.values().push(value);
    }

    pub fn dynamic_lookup(&self, name: &InternedString) -> Result<(EnvironmentRef<'h>, usize)> {
        match self.senv().lookup(name) {
            None =>
                Err(format!("undefined symbol: {:?}", name.as_str()).into()),
            Some((up_count, index)) =>
                Ok((self.clone().up(up_count), index)),
        }
    }

    pub fn dynamic_get(&self, name: &InternedString) -> Result<Value<'h>> {
        let (env, i) = self.dynamic_lookup(name)?;
        Ok(env.values().get(i))
    }

    pub fn dynamic_set(&self, name: &InternedString, value: Value<'h>) -> Result<()> {
        let (env, i) = self.dynamic_lookup(name)?;
        env.values().set(i, value);
        Ok(())
    }

    pub fn up(self, up_count: usize) -> EnvironmentRef<'h> {
        let mut env = self;
        for _ in 0..up_count {
            env = env.parent().unwrap();
        }
        env
    }

    pub fn get(&self, up_count: usize, index: usize) -> Value<'h> {
        self.clone().up(up_count).values().get(index)
    }

    pub fn set(&self, up_count: usize, index: usize, value: Value<'h>) {
        self.clone().up(up_count).values().set(index, value);
    }

    pub fn define(&self, key: InternedString, value: Value<'h>) {
        let names = self.senv().names();
        for i in 0..names.len() {
            if key == *names.get(i) {
                self.values().set(i, value);
                return;
            }
        }
        self.push(key, value);
    }

    pub fn set_expander(&self, expander: Value<'h>) {
        self.define(EXPANDER_SYMBOL.clone(), expander);
    }

    pub fn expand(&self, hs: &mut GcHeapSession<'h>, expr: Value<'h>) -> Result<Value<'h>> {
        match self.dynamic_get(&EXPANDER_SYMBOL) {
            Err(_) => Ok(expr),
            Ok(expander) => {
                let args = vec![expr];
                let tail = vm::apply(hs, expander, args)?;
                tail.eval(hs)
            }
        }
    }

    #[cfg(debug_assertions)]
    pub fn debug_assert_consistent(&self) {
        let mut env = self.clone();
        loop {
            assert_eq!(env.values().len(), env.senv().names().len());
            match (env.parent(), env.senv().parent()) {
                (None, None) => break,
                (Some(penv), Some(psenv)) => {
                    assert_eq!(penv.senv(), psenv);
                    env = penv;
                }
                _ => panic!("bad environment"),
            }
        }
    }

    #[cfg(not(debug_assertions))]
    pub fn debug_assert_consistent(&self) {}
}

/// Create and return a procedure that takes no arguments and always returns
/// the same value, k.
pub fn constant_proc<'h>(hs: &mut GcHeapSession<'h>, k: Value<'h>) -> Value<'h> {
    use compile::{self, op};

    // The procedure has an empty environment and takes no arguments.
    let env = Environment::empty(hs);
    let params_senv = env.senv().new_nested_environment(hs, vec![]);

    // Its source code is pretty straightforward.
    let insns = hs.alloc(vec![op::CONSTANT as u32, 0, op::RETURN as u32]);
    let environments = hs.alloc(vec![params_senv]);
    let constants = hs.alloc(vec![k]);
    let code = hs.alloc(compile::Code {
        insns,
        environments,
        constants,
        rest: false,
        operands_max: 1,
    });
    Value::Lambda(hs.alloc(Pair {
        car: Value::Code(code),
        cdr: Value::Environment(env),
    }))
}
