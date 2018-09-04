#![crate_name = "undisclosed_test"]
#![crate_type = "bin"]

extern crate ansi_term;
extern crate tempfile;
extern crate walkdir;

use ansi_term::Colour::{Green, Red};
use std::env;
use std::fs::remove_file;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::process::Command;
use walkdir::WalkDir;

fn main() {
    let mut pass = 0i32;
    let mut fail = 0i32;

    // for entry in WalkDir::new("../tests/pass") {
    //     let mut undisclosedc = Command::new("cargo");
    //     let entry = entry.unwrap();
    //     let mut expected = Vec::new();

    //     if entry.path().is_dir() {
    //         continue;
    //     }

    //     let mut source = String::new();

    //     println!("{}",entry.path().display());

    //     let mut file = File::open(entry.path().to_str().unwrap()).expect("File not found");

    //     println!("{:#?}",file);

    //     file.read_to_string(&mut source)
    //         .expect("something went wrong reading the file");

    //     let pattern = "// Expect :";

    //     for line in source.lines() {
    //         if let Some((index, _)) = line.match_indices(&pattern).next() {
    //             let from = index + pattern.len();
    //             let expects = line[from..].to_string();
    //             expected.push(expects);
    //         }
    //     }

    //     let mut tmpfile =  env::temp_dir();

    //     undisclosedc.args(&["run", "--", entry.path().to_str().unwrap(),"-p",tmpfile.to_str().unwrap()]);

    //     let output = undisclosedc.output().expect("failed to execute process");

    //     let output = String::from_utf8_lossy(&output.stdout);

    //     for expects in expected.iter() {
    //         if output.contains(expects) {
    //             pass += 1;
    //         } else if undisclosedc
    //             .status()
    //             .expect("failed to execute process")
    //             .success() && !output.contains(expects)
    //         {
    //             pass += 1;
    //         } else {
    //             fail += 1;
    //         }
    //     }

    //     tmpfile.push("out");

    //     let mut program = Command::new(tmpfile.to_str().unwrap());

    //     let output = program.output().expect("failed to execute process");

    //     println!("{:?}",output);
    //     let output = String::from_utf8_lossy(&output.stdout);

    //     for expects in expected {
    //         if output.contains(&expects) {
    //             pass += 1;
    //         } else if undisclosedc
    //             .status()
    //             .expect("failed to execute process")
    //             .success() && !output.contains(&expects)
    //         {
    //             pass += 1;
    //         } else {
    //             fail += 1;
    //         }
    //     }

    //     remove_file(tmpfile).expect("Couldn't remove the binary");
    // }

    // println!(
    //     "Pass:{} Fail:{}",
    //     Green.bold().paint(pass.to_string()),
    //     Red.bold().paint(fail.to_string())
    // );

    assert!(fail == 0);

    for entry in WalkDir::new("../tests/fail") {
        let mut undisclosedc = Command::new("cargo");
        let entry = entry.unwrap();

        if entry.path().is_dir() {
            continue;
        }

        let mut source = String::new();

        let mut file = File::open(entry.path().to_str().unwrap()).expect("File not found");

        file.read_to_string(&mut source)
            .expect("something went wrong reading the file");

        undisclosedc.args(&["run", "--", entry.path().to_str().unwrap()]);

        let mut expected = Vec::new();

        let pattern = "//expect:";

        for line in source.lines() {
            if let Some((index, _)) = line.match_indices(&pattern).next() {
                let from = index + pattern.len();
                let expects = line[from..].to_string();
                expected.push(expects);
            }
        }

        let output = undisclosedc.output().expect("failed to execute process");

        let output = String::from_utf8_lossy(&output.stdout);
        for expects in expected {
            if !output.contains(&expects) {
                panic!("Expected: {}", expects)
            } else {

            }
        }

        assert!(
            undisclosedc
                .status()
                .expect("failed to execute process")
                .success()
                != true
        );
    }
}
