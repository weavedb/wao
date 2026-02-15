---
name: deploy
description: "Deploy AOS scripts to AO testnet, local HyperBEAM, or remote HyperBEAM. Use when user says 'deploy', 'ship it', 'push to mainnet', or 'go live'. Not for local testing — use /test-hb."
argument-hint: "[--mainnet|--local-hb] [--lua] [lua-file ...]"
disable-model-invocation: true
allowed-tools: Bash, Read
---

Deploy AOS Lua scripts.

## Examples

```
/deploy src/counter.lua                    # deploy to testnet
/deploy --mainnet src/counter.lua          # deploy to production HyperBEAM
/deploy --local-hb --lua src/counter.lua   # deploy to local HB in Lua mode
```

## Steps

1. Run the test suite and abort if any fail:

```bash
yarn test 2>&1
```

If tests fail, stop and report. Do NOT deploy with failing tests.

2. Confirm with the user before deploying:
   - Show which scripts will be deployed
   - Show the target: testnet (default), `--mainnet` (remote HB), or `--local-hb` (local HB)
   - If `$ARGUMENTS` is provided, pass them through to the deploy script
   - If no file arguments, all `src/*.lua` files will be deployed
   - Ask for confirmation

3. Deploy:

```bash
# Testnet (default)
yarn deploy
yarn deploy src/counter.lua

# Remote HyperBEAM (production — uses push-1.forward.computer)
yarn deploy --mainnet
yarn deploy --mainnet src/counter.lua

# Local HyperBEAM (development)
yarn deploy --local-hb
yarn deploy --local-hb src/counter.lua

# Lua mode (any target)
yarn deploy --mainnet --lua
yarn deploy --local-hb --lua

# Custom node
yarn deploy --node https://push-3.forward.computer src/counter.lua
```

The script automatically:
- Finds the wallet from `.env.hyperbeam` CWD or `.wallet.json`
- Deploys each script and prints its process ID
- Reports success/failure for each file

4. Report the process IDs from the output.

## Important

- **Remote HB**: Uses `push-1.forward.computer` (NOT `push.forward.computer` which is push-only with no compute)
- **Local HB**: Requires HyperBEAM running at localhost:10001
- **Lua mode**: Faster but no `receive()` support — use `msg.reply()` pattern
- **Wallet**: Generate with `yarn keygen` if not present

## Troubleshooting

### MU 504 error on testnet
- AO testnet MU can be flaky — retry after 30 seconds
- Switch to `--mainnet` for reliable deployment

### "Wallet not found"
- Generate with `yarn keygen`
- Or check `.env.hyperbeam` CWD for existing wallet

### Process ID returned but process unreachable
- Testnet processes may take 10-30s to become available
- For HyperBEAM, verify the node is still running
