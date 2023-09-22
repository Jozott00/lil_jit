use std::cmp::{max, min};

use nom_locate::LocatedSpan;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Location {
    pub start_offset: usize,
    pub end_offset: usize,
    pub line: u32,
}

impl Location {
    pub fn new(start_off: usize, end_off: usize, line: u32) -> Location {
        Location {
            start_offset: start_off,
            end_offset: end_off,
            line,
        }
    }

    pub fn from_span(span: &LocatedSpan<&str>) -> Location {
        // TODO: calculate column from span
        Location {
            start_offset: span.location_offset(),
            end_offset: span.location_offset() + span.fragment().len(),
            line: span.location_line(),
        }
    }

    pub fn merge(&self, other: &Location) -> Location {
        Location {
            start_offset: min(self.start_offset, other.start_offset),
            end_offset: max(self.end_offset, other.end_offset),
            line: min(self.line, other.line),
        }
    }

    /// Returns the start line, end line, start column, and end column of the slice represented by self in the provided string.
    ///
    /// # Arguments
    ///
    /// * `s` - A slice of the parent string.
    pub fn get_line_and_column(&self, s: &str) -> (usize, usize, usize, usize) {
        let mut start_line = 1;
        let mut end_line = 1;
        let mut start_column = 1;
        let mut end_column = 1;

        for (i, c) in s.chars().enumerate() {
            if i == self.start_offset {
                start_line = end_line;
                start_column = end_column;
            }

            if i >= self.end_offset {
                break;
            }

            if c == '\n' {
                end_line += 1;
                end_column = 1;
            } else {
                end_column += 1;
            }
        }
        (start_line, end_line, start_column, end_column)
    }
}

impl Into<Location> for LocatedSpan<&str> {
    fn into(self) -> Location {
        Location::from_span(&self)
    }
}
