
#### 1. Create an APP

```bash
npx wao create myapp && cd myapp
```

#### 2. Run WAO Proxy

```bash
npx wao proxy
```

#### 3. Connect the Browser to the Proxy

Go to [the web app](https://preview.wao.eco) and open `Networks`, then click `Proxy`.

#### 4. Run Test

```bash
yarn test test/hyperbeam.test.js
```

#### 5. Deploy

```bash
# AO testnet (default)
yarn deploy src/counter.lua

# Local HyperBEAM (genesis-wasm)
yarn deploy --local-hb src/counter.lua

# Remote HyperBEAM (production)
yarn deploy --mainnet src/counter.lua

# Lua mode (any target)
yarn deploy --local-hb --lua src/counter.lua
```

---

### Deployment Guide

| Target | Command | Config |
|--------|---------|--------|
| AO Testnet | `yarn deploy` | `new AO().init(jwk)` |
| Local HB (Legacynet) | `yarn deploy --local-hb` | `new AO({ hb: "http://localhost:10001" }).init(jwk)` |
| Local HB (Lua) | `yarn deploy --local-hb --lua` | `new AO({ hb: url, mode: "lua" }).init(jwk)` |
| Remote HB | `yarn deploy --mainnet` | `new AO({ hb: "https://push-1.forward.computer" }).init(jwk)` |

**Wallet**: Run `yarn keygen` to generate `.wallet.json` (gitignored).

**Remote nodes**: Use `push-1` through `push-10` for full compute. `push.forward.computer` is push-only (no compute).

**HyperBEAM fork**: `git clone -b wao-final https://github.com/weavedb/HyperBEAM.git && cd HyperBEAM && rebar3 compile`

---

### Vibe Engineering

This project includes a Claude Code toolchain for agent-assisted development.

- **`CLAUDE.md`** — project context loaded automatically by the agent
- **`docs/`** — full API references read on-demand:
  - `wao-sdk.md` — WAO SDK API (AO, HB, AR, GQL, all methods)
  - `aos-lua.md` — AOS scripts (msg, ao globals, blueprints)
  - `hyperbeam-devices.md` — HyperBEAM device catalog (endpoints, config)
  - `hyperbeam-dev.md` — Building custom devices (Erlang protocol, templates)
  - `debug.md` — Troubleshooting guide (known issues, fixes)
- **`.claude/rules/`** — stack-specific rules auto-apply when editing:
  - `lua.md` for AOS script files in `src/`
  - `testing.md` for test patterns in `test/`
  - `hyperbeam.md` for Erlang device files in `HyperBEAM/`
  - `deploy.md` for deploy scripts in `scripts/`
- **Slash commands:**
  - `/test` — in-memory AOS tests (fast unit tests)
  - `/test-hb` — HyperBEAM integration tests (full stack)
  - `/deploy` — deploy to testnet, local HB, or remote HB
  - `/create-aos` — scaffold a new AOS script + test file
  - `/create-device` — scaffold a HyperBEAM Erlang device + test
  - `/debug` — troubleshoot issues (ports, processes, errors)
- **Agents** in `.claude/agents/`:
  - `builder.md` — builds features end-to-end (Lua + tests + iterate)
  - `tester.md` — runs tests and fixes failures
  - `device-builder.md` — builds HyperBEAM Erlang devices
