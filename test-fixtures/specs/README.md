# Ownership Behavior Specs

Specification tests for rs-commentary's ownership analysis. Each file tests a concept, each function tests a behavior.

## Structure

```
specs/src/
├── borrow_shared.rs    # &T behavior
├── borrow_mut.rs       # &mut T behavior
├── move_semantics.rs   # Move behavior
├── copy_types.rs       # Copy types
├── drop_nll.rs         # NLL drops
└── helpers.rs          # Test helpers (not specs)
```

## Syntax

Use `//~` comments to specify expected ownership state:

```rust
fn downgrades_owner_to_shared() {
    let x = String::new();      //~ x: owned
    let y = &x;                 //~ y: ref_shared, x: shared
}
```

### State Names

| Name | Meaning |
|------|---------|
| `owned` | Immutable owner (●●○) |
| `owned_mut` | Mutable owner (●●●) |
| `shared` | Downgraded by &T |
| `frozen` | Downgraded by &mut |
| `ref_shared` | Shared borrow (&T) |
| `ref_mut` | Mutable borrow (&mut T) |
| `moved` | Value moved out |
| `dropped` | Value dropped |

### Modifiers

```rust
//~ x: owned              # x has state owned
//~ x: shared, y: ref_shared  # Multiple vars
//~ !x                    # x should NOT be live
//~^ x: owned             # Applies to line above
```

## CRUD

### Create a test

1. Pick the concept file (e.g., `borrow_shared.rs`)
2. Add a function named like a sentence:
   ```rust
   fn multiple_shared_borrows_allowed() {
       let x = String::new();  //~ x: owned
       let y = &x;             //~ y: ref_shared, x: shared
       let z = &x;             //~ z: ref_shared
   }
   ```
3. Run: `cargo test test_borrow_shared`

### Read/find tests

```bash
# Find all tests for a concept
cat test-fixtures/specs/src/borrow_mut.rs

# Find tests mentioning "freeze"
rg "fn.*freeze" test-fixtures/specs/

# Run specific spec file
cargo test test_borrow_mut_specs
```

### Update a test

Edit the function and/or `//~` expectations, then run tests.

### Delete a test

Remove the function. No registration needed.

## Running

```bash
cargo test                          # All tests
cargo test test_borrow_shared_specs # One spec file
cargo test run_expectation_tests    # All spec files
```

## Adding a new concept

1. Create `specs/src/new_concept.rs`
2. Add `mod new_concept;` to `lib.rs`
3. Optionally add a dedicated test in `tests/expectation_tests.rs`
