# Get started

![](./docs/docs/public/images/cover.png)

## What is WizardAO?

WizardAO is the one-stop lab for engineers building on Arweave and AO. It ships tutorials, deep-dive guides, a full HyperBEAM book, and complete HyperBEAM references — everything you need to go from zero to production.

At its core is **WAO**, a lightning-fast testing framework for AOS, [HyperBEAM](https://permaweb.github.io/HyperBEAM/), and AI. Test Lua scripts 1000x faster than mainnet by emulating AO units in memory, launch HyperBEAM nodes from JS test code, build custom devices in Erlang, Rust, C++, Elixir, and Gleam, or spin up standalone local units with a single `npx wao`.

WAO also ships the **WAO SDK** — an extension of Wander (`aoconnect`) with syntactic sugar, seamless message piping, and async message result validation to drastically reduce code size.

This combination — a complete knowledge base plus sub-second testing — makes **WizardAO** the ideal stack for Agent Driven Development on AO and HyperBEAM. AI agents build reliably when they have domain knowledge to avoid hallucinating patterns and a fast feedback loop to iterate autonomously. The [HyperADD Framework](https://docs.wao.eco/add/framework) ties both together.

---

## Start Building

```
  ┌─────────────────────┬─────────────────────┐
  │                     │  Lua Scripts (AOS)  │  ← smart contracts
  │   Custom Modules    ├─────────────────────┤
  │     (WASM/Lua)      │         AOS         │  ← execution environments
  ├─────────────────────┴─────────────────────┤
  │             HyperBEAM Devices             │  ← composable infrastructure
  ├───────────────────────────────────────────┤
  │                  Arweave                  │  ← permanent storage
  └───────────────────────────────────────────┘
```

WAO provides four test environments — from instant in-memory emulation to production networks:

```
  fast ◀───────────────────────────────────────────▶ realistic

  In-Memory AOS  Local AO Units  Local HyperBEAM  Remote HyperBEAM
   emulation       npx wao          sandboxed        production
```

| Environment | Description |
|-------------|------------|
| **In-memory AOS** | AOS WASM in Node.js — no server, instant unit tests |
| **Local AO Units** | Standalone units via `npx wao` — local dev |
| **Local HyperBEAM** | Sandboxed Erlang node — full stack integration |
| **Remote HyperBEAM** | Production nodes — mainnet |

### AOS — Lua Smart Contracts
Build decentralized processes with Lua handlers running on AO.
- [Legacynet AOS →](https://docs.wao.eco/tutorials/legacynet)
- [Legacynet AOS on HyperBEAM →](https://docs.wao.eco/tutorials/legacynet-aos)
- [Mainnet AOS (WASM Device) →](https://docs.wao.eco/tutorials/mainnet-aos)
- [HyperAOS (Lua Device) →](https://docs.wao.eco/tutorials/hyperaos)

### Custom Modules — WASM/Lua Execution Modules
Build custom WASM64 or Lua execution modules that run alongside AOS on HyperBEAM.
- [Custom WASM64 in Rust →](https://docs.wao.eco/tutorials/rust-wasm64) — Custom WASM64 binary composable with AOS
- [Custom Lua Modules →](https://docs.wao.eco/tutorials/custom-lua) — Standalone Lua modules on HyperBEAM's lua@5.3a device

### HyperBEAM Devices — Composable Infrastructure
Build composable devices that extend HyperBEAM's core capabilities — each device is a building block that can be composed with others.
- [HyperBEAM →](https://docs.wao.eco/tutorials/hb)
- [Custom Devices in Erlang →](https://docs.wao.eco/tutorials/creating-devices)
- Custom Devices (Advanced) — [Rust](https://docs.wao.eco/tutorials/devices-rust) · [C++](https://docs.wao.eco/tutorials/devices-cpp) · [Elixir](https://docs.wao.eco/tutorials/devices-elixir) · [Gleam](https://docs.wao.eco/tutorials/devices-gleam)
- [The HyperBEAM Book →](https://docs.wao.eco/book) — From zero to building custom devices
- [Decoding HyperBEAM →](https://docs.wao.eco/hyperbeam/decoding-from-scratch) — Deep-dive series into internals

### Agent Driven Development
Build WAO projects using the HyperADD (Agent Driven Development) Framework.
- [Overview →](https://docs.wao.eco/add/overview)
- [Vibe Engineering →](https://docs.wao.eco/add/build)
- [HyperADD Framework →](https://docs.wao.eco/add/framework)

### Reference
- [WAO SDK API →](https://docs.wao.eco/api/overview)
- [HyperBEAM API (139 modules) →](https://docs.wao.eco/hyperbeam)

---

## Experiments
Active experiments from the lab — usable but still evolving.
- [AO in the Browser →](https://docs.wao.eco/web) — Full AO units running in your browser at [preview.wao.eco](https://preview.wao.eco)
- [HyperBEAM on Mobile →](https://docs.wao.eco/mobile) — Run a HyperBEAM node on Android / iOS
- [Running LLMs on AOS →](https://docs.wao.eco/tutorials/running-llms) — Run AI models on AO processes
