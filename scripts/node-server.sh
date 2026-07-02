#!/usr/bin/env bash
# Wrapper that injects --experimental-wasm-memory64 only on Node <24.
# Same rationale as scripts/node-test.sh — Node 24+ enables wasm-memory64
# by default and rejects the experimental flag at startup.

set -eu

major="$(node -e 'process.stdout.write(String(process.versions.node.split(".")[0]))')"

flags=()
if [ "$major" -lt 24 ]; then
    flags+=(--experimental-wasm-memory64)
fi

exec node "${flags[@]}" "$@"
