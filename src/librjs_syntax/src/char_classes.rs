use unicode_categories::UnicodeCategories;

pub trait CharClassExt : Sized + Copy {
    fn is_es_line_terminator(self) -> bool;
    fn is_es_whitespace(self) -> bool;
    fn is_es_identifier_start(self) -> bool;
    fn is_es_identifier_part(self) -> bool;
}

impl CharClassExt for char {
    fn is_es_line_terminator(self) -> bool {
        match self {
            '\n' | '\r' | '\u{2028}' | '\u{2029}' => true,
            _ => false
        }
    }

    fn is_es_whitespace(self) -> bool {
        match self {
            '\t' | '\u{000B}' | '\u{000C}' | '\u{0020}' | '\u{00A0}' | '\u{FEFF}' => true,
            c => c.is_separator_space()
        }
    }

    fn is_es_identifier_start(self) -> bool {
        match self {
            '$' | '_' | '\\' | 'a' ... 'z' | 'A' ... 'Z' => return true,
            '(' | ')' | ' ' | '[' | ']' | '.' | '0' ... '9' |
            ';' | ',' | '\n' | '+' | '-' | '*' | '/' | ':' | '<' | '>' | '&' | '=' |
            '|' | '}' => return false,
            _ => {}
        }

        //println!("es_identifier_start slow pathing: {}", self);

        self.is_letter_uppercase()   // Lu
            || self.is_letter_lowercase() // Ll
            || self.is_letter_titlecase() // Lt
            || self.is_letter_modifier()  // Lm
            || self.is_letter_other()     // Lo
            || self.is_number_letter()    // Nl
    }

    fn is_es_identifier_part(self) -> bool {
        match self {
            '\u{200C}' | '\u{200D}' => return true,
            '0' ... '9' | 'a' ... 'z' | 'A' ... 'Z' | '_' => return true,
            '.' | ' ' | ';' | '(' | '[' | ',' | '\n' | ')' | ']' | '-' | '+' |
            '*' | '>' | '<' | '/' | ':' | '}' | '&' | '=' | '|' => return false,
            _ => {}
        }

        //println!("es_identifier_part slow pathing: {}", self);

        self.is_es_identifier_start()
            || self.is_mark_nonspacing()
            || self.is_mark_spacing_combining()
            || self.is_number_decimal_digit()
            || self.is_punctuation_connector()
    }
}
