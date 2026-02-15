---
paths:
  - "scripts/**/*.js"
---

# Deploy Rules

## Deploy Command

```bash
yarn deploy                              # testnet, all src/*.lua
yarn deploy src/counter.lua              # testnet, one script
yarn deploy --mainnet                    # remote HyperBEAM (push-1)
yarn deploy --mainnet --lua              # remote HyperBEAM with Lua mode
yarn deploy --local-hb                   # local HyperBEAM (genesis-wasm)
yarn deploy --local-hb --lua             # local HyperBEAM with Lua mode
yarn deploy --node https://push-3.forward.computer  # custom node
```

## Three Deploy Targets

### Testnet (default)
Uses `new AO().init(jwk)` with aoconnect — spawns process, loads Lua code via Eval message.
- MU can be flaky (504 errors)
- `receive()` works via CU polling

### Local HyperBEAM (`--local-hb`)
Uses `new AO({ hb: "http://localhost:10001" }).init(jwk)` — deploys to a local Erlang node.
- Default: `genesis_wasm` mode (legacynet compatible)
- Add `--lua` for Lua mode (faster, no WASM, but no `receive()`)
- Requires HyperBEAM running locally

### Remote HyperBEAM (`--mainnet`)
Uses `new AO({ hb: "https://push-1.forward.computer" }).init(jwk)` — production deployment.
- Uses `push-1` through `push-10` for full compute support
- **Do NOT use `push.forward.computer`** — it's push-only (no compute)
- Add `--lua` for Lua mode
- Add `--node <url>` for a specific push node

## Wallet

- Wallet resolved from `.env.hyperbeam` CWD, or `.wallet.json` in project root
- Can also specify with `--wallet <path>` flag
- Generate with `yarn keygen` or `npx wao keygen`
- **Never commit `.wallet.json`** — it's in `.gitignore`
