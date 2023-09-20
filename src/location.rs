use std::cmp::{max, min};
use nom_locate::LocatedSpan;

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Location {
    pub start_offset: usize,
    pub end_offset: usize,
    pub line: u32,
}

impl Location {
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
            line: min(self.line, other.line)
        }
    }
}