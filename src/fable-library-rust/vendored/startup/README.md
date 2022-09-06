# `startup`: Run Rust code "before main"

[![Build Status](https://github.com/thomcc/startup/workflows/CI/badge.svg)](https://github.com/thomcc/startup/actions)
[![Docs](https://docs.rs/startup/badge.svg)](https://docs.rs/startup)
[![Latest Version](https://img.shields.io/crates/v/startup.svg)](https://crates.io/crates/startup)
![Minimum Rust Version](https://img.shields.io/badge/MSRV%201.37-blue.svg)

Tiny (no dependency, no proc macro) way to run some code before main. This is similar to the GNU C extension `__attribute__((constructor))`, or the behavior of static constructors from C++.

## Usage

```rust
startup::on_startup! {
    // Note: not all of the rust stdlib may be supported before main.
    println!("I'm running before main");
}
fn main() {
    println!("I'm inside main");
}
```

Prints:

```text
I'm running before main.
I'm inside main.
```

## Comparison with `ctor`

This crate is the moral equivalent to the [`ctor`](https://crates.io/crates/ctor) crate, although the API is completely different. The main reasons for it's existence are:

- Much faster to compile — no proc macros / syn / quote.
- More obviously safe. No support for `#[ctor]` on statics, no `#[dtor]` equivalent, and avoids a number of issues I filed with `ctor` in the past...
- Handle untested unix platforms by assuming they support *at least* the `.ctors` section. This is in line with what clang seems to do when compiling C++ static constructors. This means we should expect to have better platform support.
