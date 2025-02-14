macro_rules! start_trace {
    ($label: literal) => {
        if crate::config::verbose() {
            eprintln!("{}:", $label);
        }
    };
}

macro_rules! trace {
    ($($arg:tt)*) => {
        if crate::config::verbose() {
            eprint!($($arg)*);
        }
    };
}

macro_rules! traceln {
    ($($arg:tt)*) => {
        if crate::config::verbose() {
            eprintln!($($arg)*);
        }
    };
}

macro_rules! end_trace {
    () => {
        if crate::config::verbose() {
            eprintln!();
        }
    };
}

pub(crate) use end_trace;
pub(crate) use start_trace;
pub(crate) use trace;
pub(crate) use traceln;
