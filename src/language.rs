//! The language.

use crate::position::Position;

enum Term {
    Keyword(Position, String),
}
