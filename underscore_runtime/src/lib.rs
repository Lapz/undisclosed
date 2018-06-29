extern crate libc;
use libc::c_int;
use std::fmt::Display;



#[no_mangle]
pub extern "C" fn fib(n:c_int) -> c_int {
if (n <= 2) {
    return 1 as c_int;
  } else {
    return (fib(n - 1) + fib(n - 2)) as c_int;
  }
    
}