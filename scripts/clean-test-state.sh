#!/usr/bin/env bash
# Clean up any leftover HB / CU processes and cache dirs from prior test runs.
#
# Useful between test sweeps when sequential runs leave port conflicts or
# stale beam.smp instances behind. The wao HyperBEAM class now does this
# automatically (lsof -ti kill on cu_port + HB port), so this script is
# mostly a manual safety net for interrupted runs.

set -eu

ports=(10001 6363 4000 4001 4002 4003 4004 4100 4101 4102 4103 4104 6359)

for p in "${ports[@]}"; do
    if pids="$(lsof -ti:"$p" 2>/dev/null)"; then
        if [ -n "$pids" ]; then
            echo "kill -9 ${pids} (port $p)" >&2
            kill -9 $pids 2>/dev/null || true
        fi
    fi
done

# Belt-and-suspenders cleanups for stray Erlang VMs.
pkill -9 -f beam.smp 2>/dev/null || true
# Don't pkill node --test here — pkill -f matches the full command line, which
# would kill the parent shell if this script is invoked as part of a one-liner
# that contains "node --test" (e.g. `bash clean-test-state.sh && node --test`).
# The HB port-cleanup in startCU/shell already handles the relevant cases.

echo "test-state cleaned" >&2
