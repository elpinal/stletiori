//! The language.

#![allow(dead_code)]

use crate::position::Position;

enum Term {
    Keyword(Position, String),
}
