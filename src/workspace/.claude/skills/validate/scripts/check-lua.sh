#!/bin/bash
# Programmatic Lua pitfall checker for AOS scripts in src/
# Run: bash .claude/skills/validate/scripts/check-lua.sh

ERRORS=0

for f in src/*.lua; do
  [ -f "$f" ] || continue

  # Check Send().receive() — broken on genesis-wasm
  if grep -n 'Send(.*).receive()' "$f"; then
    echo "ERROR: $f uses Send().receive() — broken on genesis-wasm"
    ERRORS=$((ERRORS + 1))
  fi

  # Check lowercase action in Handlers.add
  if grep -nP 'Handlers\.add\([^,]+,\s*"[a-z]' "$f"; then
    echo "WARNING: $f may have lowercase Action tag"
    ERRORS=$((ERRORS + 1))
  fi

  # Check bint without require
  if grep -q 'bint(' "$f" && ! grep -q "require.*bint" "$f"; then
    echo "ERROR: $f uses bint() without require('.bint')"
    ERRORS=$((ERRORS + 1))
  fi

  # Check json without require
  if grep -qE 'json\.(encode|decode)' "$f" && ! grep -q "require.*json" "$f"; then
    echo "ERROR: $f uses json without require('json')"
    ERRORS=$((ERRORS + 1))
  fi
done

echo "---"
if [ $ERRORS -eq 0 ]; then
  echo "Lua pitfall check: PASS (0 issues)"
else
  echo "Lua pitfall check: FAIL ($ERRORS issues)"
  exit 1
fi
