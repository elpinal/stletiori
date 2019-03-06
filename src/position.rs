//! Positions.

#![allow(dead_code)]

use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub struct Point {
    line: usize,
    column: usize,
}

impl Default for Point {
    fn default() -> Self {
        Point { line: 1, column: 1 }
    }
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Point {
    fn new(line: usize, column: usize) -> Self {
        Point { line, column }
    }

    pub fn new_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    pub fn inc_column(&mut self) {
        self.column += 1;
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Position {
    start: Point,
    end: Point,
}

impl From<Point> for Position {
    fn from(p: Point) -> Self {
        Position::new(p.clone(), p)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} to {}", self.start, self.end)
    }
}

impl Position {
    pub(crate) fn new(start: Point, end: Point) -> Self {
        Position { start, end }
    }
}

pub(crate) struct Positional<T> {
    pos: Position,
    inner: T,
}

impl<T> Positional<T> {
    fn get_position(&self) -> &Position {
        &self.pos
    }
}
