# A Minimal CDP Generator for Rust 🚀

<p align="center">
	<img src="https://img.shields.io/badge/stability-wip-lightgrey.svg" alt="Work in Progress"/>
</p>

Inspired by Python's [cdp](https://py-cdp.readthedocs.io/en/latest/), this is a **minimal yet powerful** implementation of a CDP (Chrome DevTools Protocol) generator for Rust. Whether you're building tools, debugging, or automating browser tasks, this crate is designed to help you generate CDP types, commands, events, and parse CDP responses with ease.

Ready to dive in? Here's how you can get started:

---

### 🛠️ How to Use This Crate

1. **Clone the Chrome DevTools Protocol JSON Files**  
   The `./devtools-protocol/json` directory points to the JSON files from the official [Chrome DevTools Protocol](https://chromium.googlesource.com/chromium/src/+/master/third_party/blink/renderer/core/inspector/protocol.json).  
   Fetch the repository with this command:
   ```bash
   git clone https://chromium.googlesource.com/chromium/src
   ```

2. **Generate the CDP Code**  
   The `./cdp` directory is where the magic happens—this is where the generated code will be placed.  
   Run the generator with:
   ```bash
   cargo run -- ./devtools-protocol/json ./cdp
   ```

---

### 🚧 Disclaimers  
This project is still in its **early stages**, and active development is ongoing. We're working hard to make it even better, so stay tuned—**the best is yet to come!** Your feedback and contributions are welcome as we continue to refine and improve this tool.  

---

Let’s build something awesome together! 🎉
