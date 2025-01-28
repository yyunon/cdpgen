# A minimal CDP generator for Rust

Inspired from python's [cdp](https://py-cdp.readthedocs.io/en/latest/), this is a minimal implementation of a CDP generator for Rust.
This crate is intended to be used as a library to generate CDP types, commands, events and parse CDP responses.

An example of how to use this crate can be found below:

* ./devtools-protocol/json points to the json files from the
    [Chrome DevTools Protocol](https://chromium.googlesource.com/chromium/src/+/master/third_party/blink/renderer/core/inspector/protocol.json)
    You can fetch the repo via:
    ```bash
    git clone https://chromium.googlesource.com/chromium/src
    ```
* ./cdp is the output directory where the generated code will be placed

```bash
cargo run -- ./devtools-protocol/json ./cdp
```
