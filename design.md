# rs-commentary Design

## Vision

rs-commentary makes Rust's ownership system visible. It annotates source code with ownership, borrowing, and move state at each binding, helping developers understand what the borrow checker sees.

## Core Concept: Capabilities

Every binding has three capabilities:

| Capability | Meaning |
|------------|---------|
| **O** (Ownership) | Can transfer ownership (move or drop) |
| **R** (Read) | Can read the value |
| **W** (Write) | Can mutate the value |

### State Table

| State | Notation | Capabilities | Example |
|-------|----------|--------------|---------|
| Owned (mut) | `●●●` | O R W | `let mut x = val;` |
| Owned (immut) | `●●○` | O R | `let x = val;` |
| Shared (borrowed) | `●●○` | O R | `x` while `&x` exists |
| Frozen | `●○○` | O | `x` while `&mut x` exists |
| Shared borrow | `○●○` | R | `r` in `let r = &x;` |
| Mutable borrow | `○●●` | R W | `r` in `let r = &mut x;` |
| Moved/Dropped | `†` | ∅ | after move or scope end |

### Aliasing XOR Mutability

When borrowed, the original binding's capabilities are temporarily reduced:

- **Shared (`&x`)**: Original becomes read-only (O R) - can't mutate while alias exists
- **Frozen (`&mut x`)**: Original loses all access (O only) - exclusive access transferred

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      rs-commentary                          │
├─────────────────────────────────────────────────────────────┤
│  CLI                                                        │
│    rs-commentary annotate file.rs --style=diagnostic        │
├─────────────────────────────────────────────────────────────┤
│  LSP Server                                                 │
│    textDocument/inlayHint, textDocument/hover               │
├─────────────────────────────────────────────────────────────┤
│  Output Layer (src/output/)                                 │
│    Renderer trait + multiple styles                         │
│    RenderContext builds from analysis                       │
├─────────────────────────────────────────────────────────────┤
│  Utilities (src/util/)                                      │
│    StateTimeline - time-traveling state machine             │
│    Position utilities - offset/line conversion              │
├─────────────────────────────────────────────────────────────┤
│  Analysis Engine (src/analysis/)                            │
│    engine.rs - OwnershipAnalyzer, AST visitor               │
│    semantic.rs - rust-analyzer integration                  │
│      └─ Type::is_copy() for Copy detection                  │
│      └─ find_all_refs() for NLL drop detection              │
│    state.rs - BindingState machine, SetAnnotation           │
├─────────────────────────────────────────────────────────────┤
│  ra_ap_syntax / ra_ap_ide / ra_ap_hir                       │
│    Rust parser, IDE features, and HIR from rust-analyzer    │
└─────────────────────────────────────────────────────────────┘
```

### Analysis Module Structure

| File | Purpose |
|------|---------|
| `engine/mod.rs` | AST-based ownership analyzer, tracks state transitions |
| `engine/macros.rs` | Format macro parsing (println!, format!, dbg!, etc.) |
| `semantic.rs` | rust-analyzer integration for accurate type info |
| `state.rs` | Core state machine types (`BindingState`, `OwnershipEvent`) |

The engine assumes all types are non-Copy; the semantic layer corrects this
using `Type::is_copy()` from rust-analyzer's HIR. The semantic module uses
`SemanticContext` to bundle common parameters (sema, file_id, source, loop_ranges).

## State Machine

All state changes go through `BindingState::transition(event)`:

```rust
pub enum OwnershipEvent {
    SharedBorrow { by: BindingId },
    MutBorrow { by: BindingId },
    BorrowEnd { borrow_id: BindingId },
    Move { to: Option<BindingId> },
    Copy,
    ScopeExit,
}
```

```
                    ┌──────────┐
     let x = ...    │  Owned   │
    ─────────────►  │  (ORW)   │
                    └────┬─────┘
                         │
         ┌───────────────┼───────────────┐
         │               │               │
         ▼               ▼               ▼
    ┌─────────┐    ┌──────────┐    ┌──────────┐
    │   &x    │    │  &mut x  │    │  move x  │
    │ x → shr │    │ x → frz  │    │  x → ∅   │
    └────┬────┘    └────┬─────┘    └──────────┘
         │              │
         ▼              ▼
    ┌─────────┐    ┌──────────┐
    │ borrow  │    │ borrow   │
    │  ends   │    │  ends    │
    │ x → ORW │    │ x → ORW  │
    └─────────┘    └──────────┘
```

## StateTimeline (src/util/state.rs)

Time-traveling state machine for rendering. Records state at each line:

```rust
let mut timeline = StateTimeline::new();

// Record state as you process lines
timeline.record(line_num, &entries, line_text);

// Query any point in history
let state_at_line_5 = timeline.get(5);

// Get what changed on a line
let changes = timeline.get_changes(line_num);

// Detect NLL drops
let drops = timeline.get_pending_drops(line_num);

// Rewind for re-processing
timeline.rewind_to(3);

// Set semantic drop data from rust-analyzer
timeline.set_semantic_last_uses(last_uses_map);
```

Key features:
- Automatic scope reset at function boundaries
- Drop detection (variables in prev but not current)
- Change detection (new vars, state transitions)
- Scope-aware queries (don't compare across functions)
- Semantic drop override when rust-analyzer data available

## Renderer Framework (src/output/renderer.rs)

Pluggable output styles via trait:

```rust
pub trait Renderer {
    fn render(&self, ctx: &RenderContext) -> String;
}
```

### Available Styles

| Style | Description |
|-------|-------------|
| `diagnostic` | Rustc-style with line numbers, underlines (default) |
| `inline` | Comments at end of each line |
| `columnar` | Fixed columns per variable |
| `grouped` | Horizontal rules between changes |

### Example: diagnostic (default)

```
   ╭─
 1 │ fn main() {
 2 │     let mut x = String::from("hello");
   │             ─
   │             └─ x: ●●● owned mut
   │
 3 │     let r = &x;
   │         ─    ──
   │         |    └─ x: ●●○ shared (borrowed) (was owned)
   │         |
   │         └─ r: ○●○ &x
   │         └─ note: `r` dropped (last use)
   │
 4 │     println!("{}", r);
 5 │     x.push_str(" world");
   │     ─
   │     └─ x: ●●● owned mut (was shared)
   │
 6 │ }
   ╰─
```

Drop annotations are integrated with variable annotations, showing right-to-left
with vertical connectors between variables. A blank line separates annotation
blocks from the next source line.

### Adding a Renderer

1. Implement `Renderer` trait
2. Add variant to `RenderStyle` enum
3. Add case in `render_source()` match

## CLI

```bash
rs-commentary annotate file.rs                    # diagnostic (default)
rs-commentary annotate file.rs --style=inline
rs-commentary annotate file.rs --style=columnar
rs-commentary annotate file.rs --style=grouped
rs-commentary annotate file.rs --all              # include Copy types
```

## Copy Type Detection

rs-commentary uses rust-analyzer's HIR (High-level Intermediate Representation)
for accurate Copy trait detection via `Type::is_copy()`. This requires:

1. **Cargo project**: The file must be part of a cargo workspace
2. **Sysroot loading**: Standard library is auto-discovered for trait resolution

```rust
// SemanticAnalyzer checks if binding implements Copy
let is_copy = sema.type_of_binding_in_pat(pat)?.is_copy(db);
```

By default, Copy types are filtered from output. Use `--all` to show them:

```bash
rs-commentary annotate file.rs         # Non-Copy types only (default)
rs-commentary annotate file.rs --all   # Include Copy types
```

**Macro handling**:
- `println!`, `format!`, `dbg!` etc. implicitly borrow
- Both `println!("{}", x)` and `println!("{x}")` detected

## NLL Support

Borrows end at last use, not scope end:

```rust
let r = &x;           // r created, x shared
println!("{}", r);    // last use of r
                      // └─ note: `r` dropped (last use)
x.push_str("!");      // x restored to ●●●
```

### Semantic Drop Detection (src/analysis/semantic.rs)

When analyzing files in a cargo project, rs-commentary uses rust-analyzer's
`find_all_refs` API to find the actual last use of each binding:

```rust
// SemanticAnalyzer finds accurate last-use locations
let last_uses = analyzer.find_all_last_uses(file_id, source);
```

This provides accurate NLL drop points instead of heuristic detection.

### Loop-Aware Drops

Variables declared outside a loop but last used inside should drop after the
loop ends, not at their last textual use:

```rust
let digit_count = num.ilog10() + 1;  // declared here
while n > 0 {
    sum += (n % 10).pow(digit_count); // used here
    n /= 10;
}                                     // digit_count drops HERE, not inside loop
```

The `compute_drop_line()` function detects when a variable's last use is
inside a loop it wasn't declared in, and moves the drop to after the loop.

## Design Principles

1. **Cargo projects only** - Require rust-analyzer for accurate analysis
2. **Show state, not rules** - Concrete examples over abstract explanations
3. **Annotate changes** - Mark where state transitions happen
4. **Local reasoning** - One function at a time, no cross-function analysis

## Non-Goals

- Standalone file analysis (must be in cargo project)
- Cross-function analysis
- Lifetime bound visualization
- Replacing rust-analyzer

## Future Directions

- Control flow tracking through if/match branches
- Struct field tracking (`x.field` moved independently)
- Semantic tokens for color-coding by state
