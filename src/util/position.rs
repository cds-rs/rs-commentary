//! Text position utilities for converting between byte offsets and LSP positions.

use lsp_types::Position;

/// Convert a byte offset to a line/column position.
///
/// Handles UTF-8 correctly by counting characters, not bytes.
pub fn offset_to_position(text: &str, offset: usize) -> Position {
    let mut line = 0;
    let mut col = 0;
    let mut current_offset = 0;

    for ch in text.chars() {
        if current_offset >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
        current_offset += ch.len_utf8();
    }

    Position {
        line,
        character: col,
    }
}

/// Convert a line/column position to a byte offset.
///
/// Returns the byte offset corresponding to the given line and column.
pub fn position_to_offset(text: &str, position: Position) -> usize {
    let mut offset = 0;
    let mut current_line = 0;
    let mut current_col = 0;

    for ch in text.chars() {
        if current_line == position.line && current_col == position.character {
            break;
        }
        if current_line > position.line {
            break;
        }
        if ch == '\n' {
            current_line += 1;
            current_col = 0;
        } else {
            current_col += 1;
        }
        offset += ch.len_utf8();
    }

    offset
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_to_position_simple() {
        let text = "hello\nworld";
        assert_eq!(offset_to_position(text, 0), Position { line: 0, character: 0 });
        assert_eq!(offset_to_position(text, 5), Position { line: 0, character: 5 });
        assert_eq!(offset_to_position(text, 6), Position { line: 1, character: 0 });
        assert_eq!(offset_to_position(text, 11), Position { line: 1, character: 5 });
    }

    #[test]
    fn test_position_to_offset_simple() {
        let text = "hello\nworld";
        assert_eq!(position_to_offset(text, Position { line: 0, character: 0 }), 0);
        assert_eq!(position_to_offset(text, Position { line: 0, character: 5 }), 5);
        assert_eq!(position_to_offset(text, Position { line: 1, character: 0 }), 6);
        assert_eq!(position_to_offset(text, Position { line: 1, character: 5 }), 11);
    }

    #[test]
    fn test_roundtrip() {
        let text = "fn main() {\n    let x = 42;\n}";
        for offset in 0..text.len() {
            let pos = offset_to_position(text, offset);
            let back = position_to_offset(text, pos);
            assert_eq!(back, offset, "roundtrip failed for offset {}", offset);
        }
    }
}
