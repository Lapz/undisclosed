extern crate libc;
use libc::{c_char, c_int, malloc, strcat};
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
pub unsafe extern "C" fn concat(a: *const c_char, b: *const c_char) -> *const c_char {
    let a_c_str: &CStr = CStr::from_ptr(a);
    let b_c_str: &CStr = CStr::from_ptr(b);
    let mut a_str_slice: String = a_c_str.to_string_lossy().into_owned();
    let b_str_slice: String = b_c_str.to_string_lossy().into_owned();
    let buf = malloc(a_str_slice.len() + b_str_slice.len() + 1) as *mut c_char;
    a_str_slice.push_str(&b_str_slice);
    a_str_slice.push('\0');

    strcat(buf, a_str_slice.as_ptr() as *const i8);

    //
    //
    //    a_buf.push_str(&b_buf);
    //
    //    let a_buf = CString::new(a_buf).unwrap();
    //
    //
    //
    //    let ptr = a_buf.as_ptr() as *const i8;
    std::mem::forget(a_str_slice);
    std::mem::forget(b_str_slice);
    //

    buf
}
