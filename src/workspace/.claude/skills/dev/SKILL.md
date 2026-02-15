---
name: dev
description: "Start the Vite dev server for frontend development. Use when user says 'start the frontend', 'run dev server', or 'open the app'. Not for running tests — use /test-e2e for Playwright."
disable-model-invocation: true
allowed-tools: Bash, Read
---

Start the Vite dev server for frontend development.

## Steps

1. Check if the `frontend/` directory exists:

```bash
test -d frontend && echo "Frontend found" || echo "ERROR: No frontend/ directory. Run npx wao create with frontend option to scaffold one."
```

2. If `frontend/` doesn't exist, stop and tell the user to re-scaffold with the frontend option.

3. Install frontend dependencies if needed:

```bash
cd frontend && npm install
```

4. Start the Vite dev server:

```bash
cd frontend && npm run dev
```

5. Report the local URL (usually http://localhost:5173).
