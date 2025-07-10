// Not quite reg(ular) ex(pressions)...
// ...irregular expressions.667.43

// Think of this as a lighter (albeit far clunkier, brutalist) regex implementation.
// Based on https://kean.blog/post/regex-grammar.
struct Irgex {
    pattern: String
}

impl Irgex {
    fn new(pattern: &str) -> Self {
        Self { pattern: pattern.to_string() }
    }
}

// fn matches()