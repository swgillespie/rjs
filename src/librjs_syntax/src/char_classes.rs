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
            '$' | '_' | '\\' => true,
            c => c.is_letter_uppercase()   // Lu
                || c.is_letter_lowercase() // Ll
                || c.is_letter_titlecase() // Lt
                || c.is_letter_modifier()  // Lm
                || c.is_letter_other()     // Lo
                || c.is_number_letter()    // Nl
        }
    }

    fn is_es_identifier_part(self) -> bool {
        match self {
            '\u{200C}' | '\u{200D}' => true,
            c if c.is_es_identifier_start() => true,
            c if c.is_mark_nonspacing() => true,
            c if c.is_mark_spacing_combining() => true,
            c if c.is_number_decimal_digit() => true,
            c if c.is_punctuation_connector() => true,
            _ => false
        }
    }
}
