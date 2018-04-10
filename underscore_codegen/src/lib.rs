#![allow(non_camel_case_types)]

extern crate underscore_syntax as syntax;
extern crate underscore_util as util;
pub mod frame;
pub mod temp;
pub mod translate;
pub mod x86;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
