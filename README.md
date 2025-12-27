# rs-commentary

An experimental Rust ownership state visualizer. Shows O/R/W (Ownership/Read/Write) capabilities for every binding, helping you understand what the borrow checker sees.

## Status

Experimental and unsupported. This project is for educational purposes.

## What it does

```
   ╭─
 1 │ fn main() {
 2 │     let s = String::new();
   │         ─
   │         └─ s: ●●○ owned
 3 │     let r = &s;
   │         ─    ─
   │         |    └─ s: ●●○ shared (borrowed) (was owned)
   │         └─ r: ○●○ shared borrow
 4 │     println!("{}", r);
   │     ^^^^^^^^^^^^^^ note: `r` dropped (last use)
 5 │ }
   ╰─
```

### Notation

| Symbol | Meaning | Capabilities |
|--------|---------|--------------|
| `●●●` | Owned mutable | O + R + W |
| `●●○` | Owned immutable | O + R |
| `●○○` | Frozen (active `&mut`) | O only |
| `○●○` | Shared borrow (`&T`) | R only |
| `○●●` | Mutable borrow (`&mut T`) | R + W |
| `var†` | Dropped | none |

- **O** (Ownership): Can move or drop the value
- **R** (Read): Can read the value
- **W** (Write): Can mutate the value

## Installation

```bash
# Install from local source
cargo install --path .

# Or build without installing
cargo build --release
# Binary is at target/release/rs-commentary
```

## CLI Usage

```bash
# Annotate a file (default: diagnostic style)
rs-commentary annotate src/main.rs

# Pipe from stdin
cat file.rs | rs-commentary annotate

# Choose output style
rs-commentary annotate file.rs --style=inline
rs-commentary annotate file.rs --style=columnar
rs-commentary annotate file.rs --style=grouped

# Include Copy types (normally filtered)
rs-commentary annotate file.rs --all
```

## Editor integration

- [ ] TODO!

### Output Styles

- **diagnostic** (default): Rustc-style with line numbers and underlines
- **inline**: Comments at end of each line
- **columnar**: Fixed columns per variable with NLL transitions
- **grouped**: Horizontal rules between state changes

## Features

- **CLI annotation**: Multiple output styles (diagnostic, inline, columnar, grouped)
- **LSP server**: Inlay hints and hover documentation
- **NLL-style drops**: Tracks when variables are dropped at last use
- **Macro support**: Tracks borrows in `println!`, `format!`, etc. (including inline format args like `{x:#?}`)
- **State transitions**: Shows when ownership state changes (borrowed, frozen, moved)

## Runs alongside rust-analyzer

rs-commentary complements rust-analyzer. Both can run simultaneously - rust-analyzer provides completions, diagnostics, and go-to-definition, while rs-commentary adds ownership visualization.

## Limitations

- Uses heuristics for Copy/non-Copy detection (not 100% accurate)
- Single-file analysis only (no cross-function tracking)
- Best suited for learning, not production diagnostics
