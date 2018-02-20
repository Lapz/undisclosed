
#![crate_name="underscore_test"]
#![crate_type="bin"]


extern crate walkdir;

use walkdir::{WalkDir,DirEntry};

#[derive(Debug,Clone)]
struct Test {
    name:String,
    result:TestResult,
}

#[derive(Debug,Clone)]
enum TestResult {
    Pass,
    Fail,
    Expected(String),
}

macro_rules! write_test {
    ($id:ident) => {
        #[test]
        fn $id() {
            assert_eq!(1, 1);
        }
    };
}



fn main() {
    for entry in WalkDir::new("../tests/pass") {
        let entry = entry.unwrap();
        println!("{}", entry.path().display());
    }
}

