pub use std::collections::HashMap;
use std::convert::AsRef;

pub type InternedString = usize;

pub struct StringInterner {
    table: Vec<String>,
    index_map: HashMap<String, usize>,
    gensym_idx: usize
}

impl StringInterner {
    pub fn new() -> StringInterner {
        StringInterner {
            table: vec![],
            index_map: HashMap::new(),
            gensym_idx: 0
        }
    }

    pub fn intern<T: AsRef<str>>(&mut self, value: T) -> InternedString {
        let str_ref = value.as_ref();
        if let Some(idx) = self.index_map.get(str_ref) {
            return *idx;
        }

        // otherwise, it hasn't been interned yet.
        self.table.push(str_ref.to_owned());
        let last_idx = self.table.len() - 1;
        self.index_map.insert(str_ref.to_owned(), last_idx);
        last_idx
    }

    pub fn get(&self, idx: InternedString) -> &str {
        // assumption - InternedStrings can only come from the
        // string interner, so we can assume that it's a valid index
        // into the intern table.
        &self.table[idx]
    }

    pub fn gensym(&mut self) -> InternedString {
        // @ is not a legal way to start an identifier in JS,
        // so this ident will never collide with an existing ident.
        let string = format!("@gensym_{}", self.gensym_idx);
        self.gensym_idx += 1;
        self.intern(string)
    }
}
