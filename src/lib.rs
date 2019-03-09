#![feature(bind_by_move_pattern_guards)]
#![feature(try_from)]

pub mod html;
pub mod language;
pub mod parser;
pub(crate) mod position;
mod to_html;
