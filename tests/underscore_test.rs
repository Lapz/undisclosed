#![crate_name = "underscore_test"]
#![crate_type = "bin"]

extern crate walkdir;

use walkdir::{DirEntry, WalkDir};
use std::process::Command;

#[derive(Debug, Clone)]
struct Test {
    name: String,
    result: TestResult,
}

#[derive(Debug, Clone)]
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
        let mut underscorec = Command::new("cargo");
        let entry = entry.unwrap();

        if entry.path().is_dir() {
            continue;
        }

        underscorec.args(&["run", "--", entry.path().to_str().unwrap()]);

        let output = underscorec.output().expect("failed to execute process");

        println!("{}", String::from_utf8_lossy(&output.stdout));
    }
}
