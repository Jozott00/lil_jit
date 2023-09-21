use crate::location::Location;

pub struct LilError {
    pub header: String,
    pub location: Option<Location>,
    pub message: String,
}

impl LilError {
    pub fn print(self, sourcecode: &str) {
        // FIXME: implement code previews
        eprintln!(
            "{}\n\n    Code previews are not yet implemented\n\n{}\n\n",
            self.header.to_uppercase(),
            self.message
        )
    }
}
