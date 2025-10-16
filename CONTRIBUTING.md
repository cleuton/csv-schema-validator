# Developer Documentation

This document provides an overview of the **csv-schema-validator** project structure, key components, multi-crate workspace setup, and details on how the derive macro is implemented.

---

## Workspace Layout

```
csv-schema-validator/         # Root workspace
├── Cargo.toml                # Workspace manifest
├── README.md                 # User-facing README
├── csv-schema-validator      # Main library crate
│   ├── Cargo.toml            # Main crate manifest
│   └── src
│       └── lib.rs            # Public API and error type
└── csv-schema-validator-derive
    ├── Cargo.toml            # Derive crate manifest (proc-macro)
    └── src
        └── lib.rs            # Derive macro implementation
```

* **Root Cargo.toml**: defines a workspace with two members, and sets resolver to "2" for unified dependency resolution.
* **csv-schema-validator**: the runtime library exposing `ValidateCsv` and `ValidationError`.
* **csv-schema-validator-derive**: the procedural macro crate that generates validation code at compile time.

---

## Root `Cargo.toml`

```toml
[workspace]
members = [".", "csv-schema-validator-derive"]
resolver = "2"
```

* **members**: includes both the main crate and the derive crate.
* **resolver = "2"**: ensures consistent feature unification across workspace crates.

---

## Main Crate: `csv-schema-validator/Cargo.toml`

```toml
[package]
name = "csv-schema-validator"
version = "0.1.0"
edition = "2021"
description = "Derive macro to validate CSV"
authors = ["Cleuton Sampaio <cleuton@cleutonsampaio.com>"]
categories = ["parsing", "development-tools"]
license = "MIT/Apache-2.0"
repository = "https://github.com/cleuton/rustingcrab/..."
readme = "README.md"

[dependencies]
csv-schema-validator-derive = { path = "./csv-schema-validator-derive", version = "0.1.0" }
serde = { version = "1.0", features = ["derive"] }
csv = "1.3"
regex = "1.10"
once_cell = "1.19"

[dev-dependencies]
assert_matches = "1.5"
```

* **Dependency on derive crate**: imports the macro.
* **Re-exports**: in `src/lib.rs`, it `pub use ValidateCsv` and defines `ValidationError`.
* **Private re-exports**: exposes `once_cell` and `regex` in a hidden module (`__private`) so that the derive macro can refer to them.

---

## Derive Crate: `csv-schema-validator-derive/Cargo.toml`

```toml
[package]
name = "csv-schema-validator-derive"
version = "0.1.0"
edition = "2021"
description = "Procedural macro for deriving CSV schema validation"
license = "MIT"
categories = ["parsing", "development-tools"]
repository = "https://github.com/cleuton/rustingcrab/..."

[lib]
proc-macro = true

[dependencies]
syn = { version = "2.0", features = ["full", "extra-traits"] }
quote = "1.0"
proc-macro2 = "1.0"

[features]
std = []
default = ["std"]
```

* **`proc-macro = true`**: marks this crate as a procedural macro.
* **`syn`**: parses Rust syntax trees (full feature set for Meta parsing).
* **`quote`**: generates Rust code.
* **`features/std`**: by default uses the Rust standard library; can be disabled for `no_std` support if needed in the future.

---

## Main Crate `src/lib.rs`

```rust
pub use csv_schema_validator_derive::ValidateCsv;

pub use serde;
pub use csv;

#[doc(hidden)]
pub mod __private {
    pub use once_cell;
    pub use regex;
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValidationError {
    pub field: String,
    pub message: String,
}

impl std::fmt::Display for ValidationError {
    /* ... */
}

impl std::error::Error for ValidationError {}
```

* **Re-export macro**: makes `ValidateCsv` available directly.
* **Re-export dependencies**: so the derive macro can use `__private::once_cell` and `__private::regex` without version mismatches.
* **`ValidationError`**: represents a single field validation failure.

---

## Derive Implementation Overview (`csv-schema-validator-derive/src/lib.rs`)

1. **Parsing input**: uses `syn::parse_macro_input!` on `TokenStream` to get a `DeriveInput`.
2. **Field selection**:

   * Matches only `Data::Struct` with `Fields::Named`.
   * Emits a compile-time error if unsupported data structures (tuple structs, enums) are used.
3. **Validation extraction**:

   * Defines an enum `Validation` for `Range`, `Regex`, `Required`, and `Custom`.
   * The `parse_validations` method parses the contents of `#[validate(...)]` using `syn::punctuated::Punctuated`.
4. **Code generation**:

   * For each field, iterates over collected `Validation` items.
   * Uses `quote!` to emit Rust code performing the checks and pushing any `ValidationError` onto a `Vec`.
   * Generates an `impl StructName { pub fn validate_csv(&self) -> Result<(), Vec<ValidationError>> { ... } }` block.
5. **Error handling**:

   * Any parse error in the macro invokes `to_compile_error()` to surface a helpful compiler error.

---

## Test Suite (`csv-schema-validator/tests/validate_csv_test.rs`)

* Uses `assert_matches` to assert `Ok(())` or inspect errors.
* Covers all validation variants (`range`, `regex`, `required`, `custom`).

---

## Summary

This multi-crate setup cleanly separates the **runtime library** (main crate) from the **procedural macro** (derive crate). The derive macro leverages **`syn`** and **`quote`** to generate zero-overhead validation code, while the main crate exposes a simple API and error type.

Developers can quickly add CSV schema validation to their Rust structs by:

1. Adding `csv-schema-validator` to `Cargo.toml`.
2. Annotating their `#[derive(ValidateCsv)]` struct fields with `#[validate(...)]`.
3. Calling `record.validate_csv()` after deserialization.

This architecture ensures type safety, compile-time errors for unsupported patterns, and clear error reporting at runtime.

## Publishing to crates.io

This project is organized as a Cargo workspace containing two crates:

* `csv-schema-validator` — the main library
* `csv-schema-validator-derive` — the procedural macro crate

### Prerequisites

Before publishing, make sure you have:

1. A verified [crates.io](https://crates.io) account

2. A valid API token added via

   ```bash
   cargo login <YOUR_TOKEN>
   ```

3. Matching semantic versions in both crates (`0.2.0`, `0.2.1`, etc.)

### Checklist Before Publishing

Each crate must have:

* A complete `Cargo.toml` including:

  * `name`, `version`, `edition`, `description`, `license = "MIT OR Apache-2.0"`, `repository`, `readme`
  * Proper `categories` and `keywords`
  * No local `path` dependencies for published packages
* License files: `LICENSE-MIT` and `LICENSE-APACHE`
* `README.md` and, ideally, `CHANGELOG.md`
* An `include` list to limit package contents:

  ```toml
  [package]
  include = ["src/**", "Cargo.toml", "README.md", "LICENSE-*", "CHANGELOG.md"]
  ```
* (Optional) docs.rs configuration:

  ```toml
  [package.metadata.docs.rs]
  all-features = true
  ```

### Publishing Order

1. **Publish the derive crate first**

   ```bash
   cd csv-schema-validator-derive
   cargo package
   cargo publish --dry-run
   cargo publish
   ```

2. Wait a few minutes for crates.io to update its index.

3. **Update the main crate** dependency to use the published version:

   ```toml
   [dependencies]
   csv-schema-validator-derive = "0.2.0"
   ```

4. **Publish the main crate**

   ```bash
   cd ../csv-schema-validator
   cargo package
   cargo publish --dry-run
   cargo publish
   ```

### Useful Commands

Check what files will be included:

```bash
cargo package --list
```

Inspect dependency resolution:

```bash
cargo tree -i csv-schema-validator-derive
```

Build docs locally as docs.rs would:

```bash
RUSTDOCFLAGS="--cfg docsrs" cargo doc --no-deps --all-features
```

### Tags and Releases

After successful publishing:

```bash
git tag v0.2.0
git push origin v0.2.0
```

Optionally, create a GitHub release and attach changelogs or example files.

### Handling Mistakes

* You **cannot overwrite** a published version.
  Bump and release a new one (`0.2.1`, `0.2.2`, etc.).
* To temporarily disable a broken version:

  ```bash
  cargo yank --vers 0.2.0 csv-schema-validator
  ```

  To undo the yank:

  ```bash
  cargo yank --vers 0.2.0 --undo csv-schema-validator
  ```

### Local Development Tip

To test unpublished changes without editing `Cargo.toml`, use `.cargo/config.toml`:

```toml
[patch.crates-io]
csv-schema-validator-derive = { path = "csv-schema-validator-derive" }
```

---

Would you like me to append a short **“Release Process Summary”** section (3-4 lines) for the top of the file so contributors can see the overview at a glance?



