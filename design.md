# rs-commentary Design

> For API documentation, run `cargo doc --open`. This file covers design
> decisions, rationale, and future directions not suitable for rustdoc.

## Design Principles

1. **Cargo projects only**: Require rust-analyzer for accurate type analysis
2. **Show state, not rules**: Concrete examples over abstract explanations
3. **Annotate changes**: Mark where state transitions happen
4. **Local reasoning**: One function at a time, no cross-function tracking
5. **Event-first**: All AST access goes through AstIter events

## Non-Goals

- Standalone file analysis (must be in cargo project)
- Cross-function analysis
- Lifetime bound visualization
- Replacing rust-analyzer

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                     Semantic Layer                               │
│  SemanticAnalyzer: loads rust-analyzer, provides TypeOracle      │
│  - Pre-collects binding info in single pass                      │
│  - TypeOracle queries return cached results                      │
│  - Macro expansion via sema.expand_macro_call()                  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                     Event Layer (AstIter)                        │
│  - AST traversal emits typed events                              │
│  - EnterFn, ExitFn, EnterClosure, ExitClosure, Expr, Pat, etc.   │
│  - MacroCall events trigger expansion + var extraction           │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                     State Machine                                │
│  OwnershipAnalyzer.process(event)                                │
│  - Queries TypeOracle for type info (is_copy, is_scalar)         │
│  - Creates BindingInfo with is_copy from oracle                  │
│  - Emits TransferEvents for calls, macros, and let bindings      │
│  - Detects move closure captures inline via PathExpr events      │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                     Annotations                                  │
│  - SetEntry carries is_copy flag                                 │
│  - TransferEvent records context (FnArg/MacroArg/LetBinding)     │
│  - Single filtering location in renderers                        │
└─────────────────────────────────────────────────────────────────┘
```

## Analysis Module Structure

| File | Purpose |
|------|---------|
| `engine/mod.rs` | Event-driven ownership analyzer, processes AstIter events |
| `engine/macros.rs` | Macro borrow recording via rust-analyzer expansion |
| `semantic.rs` | rust-analyzer integration for type info and macro expansion |
| `state.rs` | Core state machine types (BindingState, OwnershipEvent) |
| `mod.rs` | TypeOracle trait for on-demand type queries |

## TypeOracle

The engine queries type status at each AST node during traversal via the
`TypeOracle` trait. `SemanticTypeOracle` implements this using rust-analyzer's
`find_node_at_offset_with_descend` and `type_of_binding_in_pat`.

TypeOracle is the **single source of truth** for type information:
- `is_copy()`: Does this type implement Copy?
- `is_scalar()`: Is this a primitive scalar (i32, bool, etc.)?
- `binding_kind()`: Owned, SharedRef, or MutRef?
- `expand_macro_vars()`: What variables are referenced in macro expansion?

No heuristic fallbacks. If TypeOracle returns None, we use conservative
defaults (assume non-Copy). This ensures consistent behavior.

## TransferEvent Model

All value transfers use a unified `TransferEvent`:

```rust
pub struct TransferEvent {
    pub from: String,          // Source binding name
    pub context: TransferContext,  // Where transfer happens
    pub kind: TransferKind,    // Move, Copy, SharedBorrow, MutBorrow
    pub line: u32,
    pub is_scalar_in_call: bool,
}

pub enum TransferContext {
    FunctionArg { callee: String },
    MethodReceiver { method: String },
    MacroArg { macro_name: String },
    LetBinding { to: String },
}
```

This unified model covers:
- Function call arguments: `foo(x)` → FunctionArg
- Method receivers: `x.method()` → MethodReceiver
- Macro arguments: `println!("{x}")` → MacroArg
- Let binding transfers: `let y = x` → LetBinding

## Macro Handling

Macros are **transfer boundaries**. When a variable is used in a macro:

```
println!("{x}, {y}")
  x: state-PRE → borrow → state-POST
  y: state-PRE → borrow → state-POST
```

The `macros.rs` module:
1. Detects format macros (println!, format!, dbg!, assert!, etc.)
2. Calls `TypeOracle::expand_macro_vars()` to get rust-analyzer's expansion
3. Walks the expanded AST for PathExpr nodes to find variable references
4. Records borrows via `TransferEvent` with `MacroArg` context

This replaces the previous FSM-based format string parser with accurate
semantic expansion from rust-analyzer.

## Move Closure Captures

Captures in move closures are detected **inline** during event processing:

```rust
let s = String::new();
let f = move || println!("{}", s);  // s moved here
```

When inside a move closure:
1. Track move closure scopes in `move_closure_scopes` stack
2. On PathExpr events, check if binding is from outer scope
3. If non-Copy and still owned, mark as moved into closure

This event-driven approach replaced `collect_captures_recursive()`, which
was a duplicate AST traversal.

## Loop-Aware Drops

Variables declared outside a loop but last used inside should drop after the
loop ends, not at their last textual use:

```rust
let digit_count = num.ilog10() + 1;  // declared here
while n > 0 {
    sum += (n % 10).pow(digit_count); // used here
    n /= 10;
}                                     // digit_count drops HERE, not inside loop
```

The `compute_drop_line()` function in semantic.rs detects when a variable's
last use is inside a loop it wasn't declared in, and adjusts the drop line
to after the loop.

## is_copy Propagation

Copy status flows through the annotation layer:

1. TypeOracle determines is_copy at binding creation
2. BindingInfo stores is_copy
3. SetEntry (ownership set annotation) carries is_copy
4. Renderers filter based on SetEntry.is_copy

This eliminates the need for separate `semantic_copy_types` maps and
provides a single filtering location.

## Refactoring History

The current architecture emerged from organic, exploratory iterations:

1. **Initial**: AST-only analysis with heuristic Copy detection
2. **TypeOracle**: Added on-demand type queries via rust-analyzer
3. **Event-first**: Migrated to AstIter-based traversal
4. **Unified transfers**: Consolidated CopyEvent → TransferEvent
5. **Semantic macros**: Replaced FSM parser with sema.expand_macro_call()
6. **Inline captures**: Removed collect_captures_recursive() bypass

Each iteration simplified the data pipeline and reduced special cases.

## Future Directions

- Control flow tracking through if/match branches
- Struct field tracking (`x.field` moved independently)
- Semantic tokens for color-coding by state
