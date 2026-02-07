#!/bin/bash
. ~/.asdf/asdf.sh

TESTS=(
  "json"
  "cache"
  "eunit"
  "faff"
  "message"
  "meta"
  "local_name"
  "lookup"
  "simple-pay"
  "stack"
  "relay"
  "router"
  "scheduler"
  "server"
  "patch"
  "process"
  "p4"
  "upload"
  "cron"
  "hyperbeam"
  "ans104"
  "wao-hb"
)

echo "=== TEST SWEEP $(date) ==="
for t in "${TESTS[@]}"; do
  pkill -9 -f beam.smp 2>/dev/null
  pkill -9 -f epmd 2>/dev/null
  sleep 2
  echo -n "[$t] "
  result=$(timeout 180 bash -c ". ~/.asdf/asdf.sh && node --experimental-wasm-memory64 --test test/hyperbeam/${t}.test.js 2>&1" | grep -E "^# pass|^# fail" | tr '\n' ' ')
  echo "$result"
done
echo "=== DONE ==="
