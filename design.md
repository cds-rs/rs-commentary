# rs-commentary Design

> For API documentation, run `cargo doc --open`. This file covers design
> decisions, rationale, and future directions not suitable for rustdoc.

## Design Principles

1. **Cargo projects only**: Require rust-analyzer for accurate type analysis
2. **Show state, not rules**: Concrete examples over abstract explanations
3. **Annotate changes**: Mark where state transitions happen
4. **Local reasoning**: One function at a time, no cross-function tracking

## Non-Goals

- Standalone file analysis (must be in cargo project)
- Cross-function analysis
- Lifetime bound visualization
- Replacing rust-analyzer

## Analysis Module Structure

| File | Purpose |
|------|---------|
| `engine/mod.rs` | AST-based ownership analyzer, tracks state transitions |
| `engine/macros.rs` | Format macro parsing (println!, format!, dbg!, etc.) |
| `semantic.rs` | rust-analyzer integration for accurate type info |
| `state.rs` | Core state machine types (BindingState, OwnershipEvent) |
| `mod.rs` | TypeOracle trait for on-demand type queries |

## TypeOracle

The engine queries Copy status at each AST node during traversal via the
`TypeOracle` trait. `SemanticTypeOracle` implements this using rust-analyzer's
`find_node_at_offset_with_descend` and `type_of_binding_in_pat`.

This fixes cases where AST analysis alone fails:
- For-loop variables: `for x in iter` where `x` might be `&mut T`
- Generic parameters resolved through trait bounds
- Type aliases and newtype patterns

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

## Macro Handling

Format macros implicitly borrow their arguments:
- `println!("{}", x)` borrows x
- `println!("{x}")` inline format also borrows x
- `dbg!(x)` borrows x (unless x is Copy, then it copies)

The `engine/macros.rs` module parses format strings to detect which variables
are referenced, generating synthetic borrow events.

## Future Directions

- Control flow tracking through if/match branches
- Struct field tracking (`x.field` moved independently)
- Semantic tokens for color-coding by state
