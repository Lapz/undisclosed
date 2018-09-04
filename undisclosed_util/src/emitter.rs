//! Error reporting that reports all compiler errors.
use ansi_term::Colour::{Blue, Fixed, Red, Yellow};
use pos::Span;
use pos::EMPTYSPAN;
use std::cell::RefCell;
use std::fmt::{self, Display};
use std::iter::repeat;
use std::rc::Rc;

#[derive(Debug)]
pub struct Diagnostic {
    msg: String,
    level: Level,
    span: Span,
}

#[derive(Debug, PartialEq)]
pub enum Level {
    Warn,
    Error,
}

impl Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Level::Warn => write!(f, "{}", Yellow.bold().paint("warning")),
            Level::Error => write!(f, "{}", Red.bold().paint("error")),
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct Reporter {
    diagnostics: Rc<RefCell<Vec<Diagnostic>>>,
}

impl Reporter {
    pub fn new() -> Reporter {
        Default::default()
    }

    /// Checks if errors have occured
    pub fn had_error(&self) -> bool {
        !self.diagnostics.borrow().is_empty()
    }

    pub fn global_error(&self, msg: &str) {
        self.diagnostics.borrow_mut().push(Diagnostic {
            msg: msg.into(),
            span: EMPTYSPAN,
            level: Level::Error,
        })
    }

    pub fn error<T: Into<String>>(&self, msg: T, span: Span) {
        self.diagnostics.borrow_mut().push(Diagnostic {
            msg: msg.into(),
            span,
            level: Level::Error,
        })
    }
    /// Remove the last entered erro
    pub fn pop_error(&mut self) {
        self.diagnostics.borrow_mut().pop();
    }

    pub fn warn(&self, msg: &str, span: Span) {
        self.diagnostics.borrow_mut().push(Diagnostic {
            msg: msg.into(),
            span,
            level: Level::Warn,
        })
    }

    pub fn emit(&self, input: &str) {
        for diagnostic in self.diagnostics.borrow().iter() {
            print(input, diagnostic)
        }
    }
}

pub fn print(input: &str, d: &Diagnostic) {
    match *d {
        Diagnostic {
            ref msg,
            ref span,
            ref level,
        } => {
            println!("{}: {}", level, Fixed(252).bold().paint(msg.clone()));
            print_highlight(input, span, level, 2)
        }
    }
}

fn print_highlight(input: &str, span: &Span, level: &Level, detail: i32) {
    let prefix = Blue.paint("| ");

    let span = span;

    let start_line = if span.start.line >= detail {
        span.start.line - detail
    } else {
        0
    };

    for (idx, line) in input.lines().enumerate().skip(start_line as usize) {
        let line = line;
        let line_idx = idx + 1;
        println!("{:>4} {}{}", line_idx, prefix, line);
        if line_idx == span.start.line as usize {
            let end = if line_idx == span.end.line as usize {
                span.end.column as usize
            } else {
                line.len()
            };
            let carets = repeat_string("^", end - span.start.column as usize + 1);

            let carets = match *level {
                Level::Warn => Yellow.bold().paint(carets),
                Level::Error => Red.bold().paint(carets),
            };

            let whitespace = repeat_string(" ", span.start.column as usize - 1);
            println!("     {}{}{}", prefix, whitespace, carets);
        } else if line_idx == span.end.line as usize {
            let carets = repeat_string("^", span.end.column as usize);
            let carets = match *level {
                Level::Warn => Yellow.bold().paint(carets),
                Level::Error => Red.bold().paint(carets),
            };
            println!("     {}{}", prefix, carets);
        } else if line_idx > span.start.line as usize
            && line_idx < span.end.line as usize
            && !line.is_empty()
        {
            let carets = repeat_string("^", line.len());
            let carets = match *level {
                Level::Warn => Yellow.bold().paint(carets),
                Level::Error => Red.bold().paint(carets),
            };
            println!("     {}{}", prefix, carets);
        }

        if line_idx >= span.end.line as usize + detail as usize {
            break;
        }
    }
}

fn repeat_string(s: &str, count: usize) -> String {
    repeat(s).take(count).collect()
}
