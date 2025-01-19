macro_rules! start_trace {
    ($label: literal, $depth: expr) => {
        if crate::config::verbose() {
            print!("\n{}{}:", "  ".repeat($depth), $label);
        }
    };
}

macro_rules! trace {
    ($value: expr) => {
        if crate::config::verbose() {
            print!(" {}", $value);
        }
    };
}

macro_rules! end_trace {
    () => {
        if crate::config::verbose() {
            println!();
        }
    };
}

pub(crate) use end_trace;
pub(crate) use start_trace;
pub(crate) use trace;
