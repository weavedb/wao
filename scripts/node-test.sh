#!/usr/bin/env bash
# Wrapper that injects --experimental-wasm-memory64 only on Node <24.
# Node 24+ enables wasm-memory64 by default and rejects the experimental
# flag at startup. Older Node still needs it for AOS WAMR / ao-loader.
#
# All arguments are forwarded to `node --test`.

set -eu

major="$(node -e 'process.stdout.write(String(process.versions.node.split(".")[0]))')"

flags=()
if [ "$major" -lt 24 ]; then
    flags+=(--experimental-wasm-memory64)
fi

exec node "${flags[@]}" --test --test-concurrency=1 "$@"
