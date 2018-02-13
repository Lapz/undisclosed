extern crate underscore_syntax;

extern crate underscore_util;

use underscore_util::pos::{Position, Span};
use underscore_util::emitter::Reporter;

fn main() {
    let reporter = Reporter::new();

    reporter.error(
        "Expected !",
        Span {
            start: Position {
                line: 1,
                column: 1,
                absolute: 1,
            },
            end: Position {
                line: 1,
                column: 4,
                absolute: 5,
            },
        },
    );

    reporter.emit("Hello World");
}
