extern crate libc;
use libc::c_int;

#[no_mangle]
pub extern "C" fn puti(x:c_int)  -> c_int {
    println!("{}", x);
    x
}
