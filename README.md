# rs-commentary

A Rust ownership state visualizer. Shows O/R/W (Ownership/Read/Write) capabilities for every binding, helping you understand what the borrow checker sees.

## Status

Experimental. This project is for educational purposes and learning Rust's ownership model. It is probably wrong, in fact, assume it is!

## What it does

Annotates Rust code with ownership state at each line:

```
   ╭─
 1 │ fn main() {
 2 │     let s = String::new();
   │         ─
   │         └─ s: ●●○ owned
 3 │     let r = &s;
   │         ─    ─
   │         |    └─ s: ●●○ shared (borrowed)
   │         └─ r: ○●○ shared borrow
 4 │     println!("{}", r);
   │     ^^^^^^^^^^^^^^ note: `r` dropped (last use)
 5 │     takes_ref(&s);
   │               ─
   │               └─ s: borrowed → takes_ref
 6 │ }
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
cargo install --path .

# Or build without installing
cargo build --release
# Binary at target/release/rs-commentary
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

# Interactive HTML visualization in browser
rs-commentary annotate file.rs --style=html --serve
rs-commentary annotate file.rs --style=html --serve --port 3000

# Include Copy types (normally filtered out)
rs-commentary annotate file.rs --all
```

### Output Styles

| Style | Description | Valid Rust? |
|-------|-------------|-------------|
| `diagnostic` | Rustc-style with line numbers and underlines (default) | No |
| `inline` | Comments at end of each line | Yes |
| `columnar` | Fixed columns per variable | Yes |
| `grouped` | Horizontal rules between changes | Yes |
| `set-notation` | Tutorial-style: `main{mut x, r(&x)}` | No |
| `vertical-spans` | Box-drawing brackets for borrow lifetimes | No |
| `html` | Interactive reveal.js slides with state panels | N/A |
| `validated` | Ownership state + rust-analyzer diagnostics | No |

"Valid Rust" styles produce compilable output; annotations live in `//` comments.

### HTML Visualization

The `--style=html --serve` option launches an interactive slideshow:

- Step through code line by line with arrow keys
- Side panel shows current ownership state for all tracked variables
- Highlights state transitions: new bindings, borrows, drops
- Uses reveal.js for smooth navigation

## LSP Server

rs-commentary includes an LSP server for editor integration:

```bash
# Start LSP server (also the default when no command given)
rs-commentary lsp
```

Provides:
- Inlay hints showing ownership state inline
- Hover documentation with O/R/W capabilities

Configure your editor to use `rs-commentary lsp` as a language server alongside rust-analyzer.

## Features

- **Multiple output styles**: Terminal, HTML slides, LSP hints
- **NLL-aware drops**: Tracks when variables are dropped at last use via rust-analyzer
- **Accurate Copy detection**: Uses rust-analyzer's type system to filter Copy types
- **Call-site transfers**: Shows what happens at function calls (`copied → fn`, `mut borrowed → fn`)
- **Macro support**: Tracks borrows in `println!`, `format!`, `vec!`, etc.
- **State transitions**: Shows when ownership state changes (borrowed, frozen, restored)
- **Borrow tracking**: Visualizes when borrows end and owners regain capabilities

## Architecture

```
src/
├── analysis/       # Ownership analysis engine
│   ├── engine/     # Core analyzer (split into modules)
│   │   ├── mod.rs  # OwnershipAnalyzer, state machine application
│   │   └── macros.rs # Format macro analysis (println!, format!, etc.)
│   ├── semantic.rs # rust-analyzer integration for NLL drops
│   └── state.rs    # Binding states and transitions
├── output/         # Rendering
│   ├── renderers/  # Each output style (html, inline, columnar, etc.)
│   ├── context.rs  # RenderContext with StateTimeline
│   └── helpers.rs  # Shared annotation utilities
├── util/
│   ├── ast_visitor.rs # AstIter: lazy iterator over AST events
│   └── state.rs       # StateTimeline: single source of truth for state
├── execution/      # Call analysis (function registry, call detection)
└── lsp/            # LSP server implementation
```

## Requirements

- Files should be part of a Cargo project for full semantic analysis
- Standalone files work but without NLL drop detection or Copy type filtering

## Limitations

- Single-file analysis only; no cross-function ownership tracking
- Best suited for learning, not production diagnostics
- Some complex patterns (async, unsafe) may not be fully represented

## Testing

```bash
cargo test                           # Run all tests
cargo test --test expectation_tests  # Run spec tests
```

Specs use `//~` comments to define expected ownership states inline:

```rust
// test-fixtures/specs/src/borrow_shared.rs
fn downgrades_owner_to_shared() {
    let x = String::new();      //~ x: owned
    let y = &x;                 //~ y: ref_shared, x: shared
}
```

See [test-fixtures/specs/README.md](test-fixtures/specs/README.md) for how to create, find, and update tests.

## Runs alongside rust-analyzer

rs-commentary complements rust-analyzer. Both can run simultaneously: rust-analyzer provides completions, diagnostics, and navigation; rs-commentary adds ownership visualization.
