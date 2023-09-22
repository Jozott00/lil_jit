use crate::location::Location;
use nom::Slice;

#[derive(Debug)]
pub struct LilError {
    pub header: String,
    pub location: Option<Location>,
    pub message: String,
}

impl LilError {
    pub fn print(self, sourcecode: &str) {
        // FIXME: implement code previews
        eprintln!("{}\n", self.header.to_uppercase());

        if let Some(loc) = self.location {
            let (startline, endline, startcol, endcol) = loc.get_line_and_column(sourcecode);
            let mut lines = sourcecode.lines();

            if startline == endline {
                eprintln!("{}", lines.nth(startline - 1).unwrap());
                eprint!("{}", " ".repeat(startcol - 1));
                eprintln!("{}\n", "^".repeat(endcol - startcol));
            } else {
                eprintln!("    No muliline preview available\n\n");
            }
        } else {
            eprintln!("    No preview available\n\n");
        }

        eprintln!("{}\n\n", self.message);
    }
}
