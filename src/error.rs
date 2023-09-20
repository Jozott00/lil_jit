use crate::location::Location;

pub struct LilError {
    pub header: String,
    pub location: Option<Location>,
    pub message: String,
}