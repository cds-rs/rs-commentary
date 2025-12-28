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
rs-commentary annotate file.rs --style=html

# Serve HTML visualization in browser
rs-commentary annotate file.rs --style=html --serve
rs-commentary annotate file.rs --style=html --serve --port 3000

# Include Copy types (normally filtered)
rs-commentary annotate file.rs --all
```

## Editor integration

- [ ] TODO!

### Output Styles

| Style | Description | Valid Rust? |
|-------|-------------|-------------|
| `diagnostic` | Rustc-style with line numbers and underlines (default) | No |
| `inline` | Comments at end of each line | Yes |
| `columnar` | Fixed columns per variable | Yes |
| `grouped` | Horizontal rules between changes | Yes |
| `set-notation` | Tutorial-style: `main{mut x, r(&x)}` | No |
| `vertical-spans` | Box-drawing brackets for borrow lifetimes | No |
| `html` | Interactive browser visualization | N/A |
| `validated` | Ownership state + rust-analyzer errors | No |

"Valid Rust" styles can be compiled - annotations are in `//` comments.

## Features

- **CLI annotation**: Multiple output styles (diagnostic, inline, columnar, html, etc.)
- **HTML visualization**: Interactive browser view with `--serve`
- **LSP server**: Inlay hints and hover documentation
- **NLL-aware drops**: Tracks when variables are dropped at last use (via rust-analyzer)
- **Accurate Copy detection**: Uses rust-analyzer's type system
- **Macro support**: Tracks borrows in `println!`, `format!`, etc.
- **State transitions**: Shows when ownership state changes (borrowed, frozen, moved)

## Runs alongside rust-analyzer

rs-commentary complements rust-analyzer. Both can run simultaneously - rust-analyzer provides completions, diagnostics, and go-to-definition, while rs-commentary adds ownership visualization.

## Requirements

- Files must be part of a Cargo project (uses rust-analyzer for analysis)

## Limitations

- Single-file analysis only (no cross-function tracking)
- Best suited for learning, not production diagnostics
