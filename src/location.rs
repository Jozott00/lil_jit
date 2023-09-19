
struct Location<'a> {
    pub offset: usize,
    pub line: u32,
    pub column: u32,

    slice: &'a str
}