//! Poisoning patterns for debugging and assertions.

use std::mem;
use std::ptr;

macro_rules! poison {
    (
        $(
            $( #[$attr:meta] )*
            $name:ident => $pattern:expr,
        )*
    ) => {
        /// Constants used as poison patterns.
        pub mod constants {
            $(
                $( #[$attr] )*
                #[allow(non_upper_case_globals)]
                pub const $name: u8 = $pattern;
            )*
        }

        /// Functions used to paint poison patterns.
        pub(crate) mod paint {
            use std::ptr;

            $(
                $(#[$attr])*
                #[cfg(any(debug_assertions, test))]
                #[inline(always)]
                pub unsafe fn $name<T>(object_ptr: *mut T) {
                    ptr::write_bytes(object_ptr, $pattern, 1);
                }

                $(#[$attr])*
                #[cfg(not(any(debug_assertions, test)))]
                #[inline(always)]
                pub unsafe fn $name<T>(_: *mut T) {}
            )*
        }

        /// Assert that the given pointed-to object is not painted with any of
        /// our poison patterns.
        #[cfg(any(debug_assertions, test))]
        pub unsafe fn assert_is_not_poisoned<T>(object_ptr: *const T) {
            let n = mem::size_of::<T>();
            let p = object_ptr as *const u8;
            $(
                assert!(
                    !(0..n).map(|i| p.offset(i as isize))
                        .map(|p| ptr::read(p))
                        .all(|byte| byte == $pattern),
                    concat!("Found poison pattern '", stringify!($name), "' at {:p}"),
                    p
                );
            )*
        }

        /// Assert that the given pointed-to object is not painted with a poison
        /// pattern.
        #[cfg(not(any(debug_assertions, test)))]
        #[inline(always)]
        pub unsafe fn assert_is_not_poisoned<T>(object_ptr: *const T) {}
    }
}

poison! {
    /// Poison pattern for objects after they have been swept.
    swept => 0xf4,
}
