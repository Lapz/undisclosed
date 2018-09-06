extern crate libc;
use libc::{c_char, c_int,malloc,strcat};
use std::ffi::{CStr, CString};
use std::str;

#[no_mangle]
pub extern "C" fn fib(n: c_int) -> c_int {
    if n <= 2 {
        return 1 as c_int;
    } else {
        return (fib(n - 1) + fib(n - 2)) as c_int;
    }
}

#[no_mangle]
pub unsafe extern "C"  fn concat(a: *const c_char, b: *const c_char) ->    *const c_char {
    let a_c_str: &CStr =  CStr::from_ptr(a) ;
    let b_c_str: &CStr =  CStr::from_ptr(b) ;
    let a_str_slice: &str = a_c_str.to_str().unwrap();
    let b_str_slice: &str = b_c_str.to_str().unwrap();
    let buf  = malloc(a_str_slice.len() + b_str_slice.len() + 1) as *mut c_char;
    strcat(buf,a_c_str.as_ptr());
    strcat(buf,b_c_str.as_ptr());
    strcat(buf,b"\0".as_ptr() as *const i8);

//
//
//    a_buf.push_str(&b_buf);
//
//    let a_buf = CString::new(a_buf).unwrap();
//
//
//
//    let ptr = a_buf.as_ptr() as *const i8;
//    std::mem::forget(a_buf);
//    std::mem::forget(b_buf);
//

    buf

}

