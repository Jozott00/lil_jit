use crate::location::Location;

#[derive(Debug)]
pub struct LilError {
    pub header: String,
    pub location: Option<Location>,
    pub message: String,
}

impl LilError {
    pub fn print(self, sourcecode: &str) {
        eprintln!("{}\n", self.header.to_uppercase());

        if let Some(loc) = self.location {
            let (startline, endline, startcol, endcol) = loc.get_line_and_column(sourcecode);
            let mut lines = sourcecode.lines();

            if startline == endline {
                eprint!("{:2}| ", startline);
                eprintln!("{}", lines.nth(startline - 1).unwrap());
                eprint!("{}", " ".repeat(startcol - 1 + 4));
                eprintln!("{}\n", "^".repeat(endcol - startcol));
            } else {
                let mut lines = lines.skip(startline - 1);
                for n in startline..=endline {
                    match lines.next() {
                        Some(line) => {
                            eprint!("{:2}> ", n);
                            eprintln!("{}", line);
                        }
                        None => {}
                    }
                }
                eprintln!();
            }
        } else {
            eprintln!("    No preview available\n");
        }

        eprintln!("{}\n\n", self.message);
    }
}
