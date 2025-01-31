macro_rules! start_trace {
    ($label: literal) => {
        if crate::config::verbose() {
            eprint!("\n{}:", $label);
        }
    };
}

macro_rules! trace {
    ($value: expr) => {
        if crate::config::verbose() {
            eprint!(" {}", $value);
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
