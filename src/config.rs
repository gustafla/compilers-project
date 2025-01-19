use std::cell::RefCell;

use clap::Args;

#[derive(Args, Debug, Clone)]
pub struct Config {
    #[arg(short, long, global = true)]
    pub verbose: bool,
}

thread_local! {
    pub static CONFIG: RefCell<Option<Config>> = const { RefCell::new(None) };
}

pub fn configure(config: Config) {
    CONFIG.with_borrow_mut(|cfg| *cfg = Some(config));
}

pub fn verbose() -> bool {
    CONFIG.with_borrow(|cfg| match cfg {
        Some(cfg) => cfg.verbose,
        None => true,
    })
}
