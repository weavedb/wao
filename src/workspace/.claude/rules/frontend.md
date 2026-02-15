---
paths:
  - "frontend/**/*.jsx"
  - "frontend/**/*.tsx"
  - "frontend/**/*.js"
---

# Frontend Rules (WAO Browser Apps)

For full SDK reference, read `docs/wao-sdk.md` (browser section).

## Browser Imports

Use `wao/web` for browser code, NOT `wao/test`:

```js
import { AO, AR } from "wao/web"
```

`wao/test` is for Node.js testing only. `wao/web` works in the browser with ArConnect.

## ArConnect Wallet Connection

```js
await window.arweaveWallet.connect([
  "ACCESS_ADDRESS",
  "SIGN_TRANSACTION",
  "ACCESS_PUBLIC_KEY",
])
const addr = await window.arweaveWallet.getActiveAddress()
```

Always check `window.arweaveWallet` exists before calling connect.

## AO Browser Setup

```js
const ao = new AO()
await ao.init(window.arweaveWallet)
const p = ao.p(processId)

// Send message
const { out } = await p.msg("Action", { Tag: "value" })

// Dry-run (read-only)
const { out } = await p.msg("Action", { Tag: "value" })
```

## Vite Dev Server

```bash
cd frontend && npm run dev      # start dev server (port 5173)
cd frontend && npm run build    # production build
cd frontend && npm run preview  # preview production build
```

## Testing

```bash
cd frontend && npm run test:unit   # vitest component tests
cd frontend && npm run test:e2e    # playwright E2E tests
```

## Common Patterns

- Use `wao.js` wrapper for AO initialization and wallet connection
- Process IDs should be environment variables or config
- ArConnect may not be installed — show a helpful message
- All AO operations are async — use proper loading states
