//! Precise tracing of particular operations with OSX Instruments' "Points of
//! Interests" tool.

#[cfg(feature = "signpost")]
extern crate signpost;

macro_rules! define_signpost {
    ( $code:expr, $name:ident ) => {
        #[cfg(feature = "signpost")]
        pub struct $name(self::signpost::AutoTrace<'static>);

        #[cfg(feature = "signpost")]
        impl $name {
            pub fn new() -> Self {
                static ARGS: &'static [usize; 4] = &[0, 0, 0, $code];
                $name(self::signpost::AutoTrace::new($code, ARGS))
            }
        }

        #[cfg(not(feature = "signpost"))]
        pub struct $name;

        #[cfg(not(feature = "signpost"))]
        impl $name {
            #[inline(always)]
            pub fn new() -> Self {
                $name
            }
        }
    }
}

define_signpost!(100, Marking);
define_signpost!(101, ClearMarkBits);
define_signpost!(200, Sweeping);
define_signpost!(300, Dropping);
