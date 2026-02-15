import { useState, useEffect } from "react"

// ═══════════════════════════════════════════════════════════════
// Demo data
// ═══════════════════════════════════════════════════════════════

const DEMO_DATA = {
  feature: "Token Transfer App",
  tasks: [
    { id: 1, name: "Plan feature", type: "plan", status: "done", skill: "/plan", done_when: "plan.md and tasks.json created", files: ["plan.md", "tasks.json"], started_at: "2026-02-15T10:00:00Z", completed_at: "2026-02-15T10:02:14Z" },
    { id: 2, name: "Enhance AOS token handlers", type: "aos", status: "done", skill: "/build-aos", done_when: "Mint, transfer, balance handlers pass all tests", files: ["src/token.lua", "src/registry.lua"], started_at: "2026-02-15T10:02:14Z", completed_at: "2026-02-15T10:08:47Z" },
    { id: 3, name: "Write in-memory AOS token tests", type: "aos-test", status: "done", skill: "/build-aos", done_when: "All token unit tests pass", files: ["test/token.test.js", "test/registry.test.js"], started_at: "2026-02-15T10:08:47Z", completed_at: "2026-02-15T10:14:22Z" },
    { id: 4, name: "Write AOS HyperBEAM integration tests for token", type: "aos-integration", status: "done", skill: "/test-hb", done_when: "Token operations work on HyperBEAM", files: ["test/hyperbeam-token.test.js"], started_at: "2026-02-15T10:14:22Z", completed_at: "2026-02-15T10:19:05Z" },
    { id: 5, name: "Build HyperBEAM token device + eunit tests", type: "device", status: "done", skill: "/build-device", done_when: "Device compiles and eunit passes", files: ["HyperBEAM/src/dev_token.erl"], started_at: "2026-02-15T10:19:05Z", completed_at: "2026-02-15T10:28:33Z" },
    { id: 6, name: "Write device JS integration tests", type: "device-integration", status: "done", skill: "/test-device", done_when: "JS SDK can call device via HTTP", files: ["test/token-device.test.js"], started_at: "2026-02-15T10:28:33Z", completed_at: "2026-02-15T10:33:41Z" },
    { id: 7, name: "Build frontend components", type: "frontend", status: "done", skill: "/build-frontend", done_when: "All components render correctly", files: ["frontend/src/App.jsx", "frontend/src/components/TransferForm.jsx"], started_at: "2026-02-15T10:33:41Z", completed_at: "2026-02-15T10:41:09Z" },
    { id: 8, name: "Write frontend unit tests — debugging 1 failure", type: "frontend-test", status: "in_progress", skill: "/build-frontend", done_when: "Vitest passes 100%", files: ["frontend/src/__tests__/App.test.jsx", "frontend/src/__tests__/TransferForm.test.jsx"], started_at: "2026-02-15T10:41:09Z" },
    { id: 9, name: "Write frontend E2E integration tests", type: "frontend-integration", status: "pending", skill: "/test-e2e", done_when: "Playwright E2E passes with live HyperBEAM", files: ["frontend/e2e/token-transfer.spec.js"] },
    { id: 10, name: "Generate README", type: "readme", status: "pending", skill: "/readme", done_when: "README.md covers setup, usage, and API" },
    { id: 11, name: "Final validation", type: "validate", status: "pending", skill: "/validate", done_when: "All gates pass, no Lua pitfalls" },
  ],
}

const DEMO_FILES = [
  { path: "package.json", size: 687 }, { path: "plan.md", size: 2948 }, { path: "README.md", size: 1843 }, { path: "tasks.json", size: 6541 },
  { path: "frontend/e2e/debug.spec.js", size: 0 }, { path: "frontend/e2e/token-transfer.spec.js", size: 9216 },
  { path: "frontend/src/App.css", size: 3072 }, { path: "frontend/src/App.jsx", size: 2048 }, { path: "frontend/src/index.css", size: 417 },
  { path: "frontend/src/main.jsx", size: 129 }, { path: "frontend/src/test-setup.js", size: 0 },
  { path: "frontend/src/__tests__/App.test.jsx", size: 1621 }, { path: "frontend/src/__tests__/TransferForm.test.jsx", size: 4136 },
  { path: "frontend/src/components/BalanceDisplay.jsx", size: 640 }, { path: "frontend/src/components/TokenInfo.jsx", size: 730 },
  { path: "frontend/src/components/TransactionStatus.jsx", size: 159 }, { path: "frontend/src/components/TransferForm.jsx", size: 0 },
  { path: "frontend/src/components/WalletConnect.jsx", size: 610 },
  { path: "frontend/src/hooks/useToken.js", size: 3148 }, { path: "frontend/src/hooks/useWallet.js", size: 1440 },
  { path: "HyperBEAM/src/dev_token.erl", size: 4800 },
  { path: "scripts/deploy.js", size: 783 },
  { path: "src/counter.lua", size: 318 }, { path: "src/registry.lua", size: 1900 }, { path: "src/token.lua", size: 2948 },
  { path: "test/aos.test.js", size: 2700 }, { path: "test/hyperbeam-token.test.js", size: 2700 }, { path: "test/hyperbeam.test.js", size: 3534 },
  { path: "test/registry.test.js", size: 4200 }, { path: "test/token-device.test.js", size: 1228 }, { path: "test/token.test.js", size: 1900 },
]

const DEMO_PLAN = `# Token Transfer App

## Overview
A decentralized token transfer application built on AOS and HyperBEAM.

## AOS Scripts
- \`src/token.lua\` — Token handler with mint, transfer, balance
- \`src/registry.lua\` — Token registry for discoverability

## Frontend
- React SPA with Vite
- WalletConnect component for ArConnect
- TransferForm with validation
- BalanceDisplay with real-time updates

## Test Plan
- Unit tests: in-memory AOS token operations
- Integration: HyperBEAM token device
- E2E: Full browser flow with Playwright
`

const DEMO_CONTENT = {
  "package.json": `{
  "name": "token-transfer-app",
  "version": "0.0.1",
  "type": "module",
  "scripts": {
    "test": "node --experimental-wasm-memory64 --test --test-concurrency=1",
    "deploy": "node scripts/deploy.js",
    "start": "trap 'kill $(jobs -p)' EXIT; node dashboard/server.js & cd dashboard && npx vite",
    "keygen": "node scripts/keygen.js"
  },
  "dependencies": {
    "hbsig": "0.3.0",
    "wao": "0.40.0"
  }
}`,
  "plan.md": DEMO_PLAN,
  "README.md": `# Token Transfer App

A decentralized token transfer application built on AOS and HyperBEAM.

## Quick Start

\`\`\`bash
yarn install
yarn keygen            # generate wallet
yarn test              # run all tests
yarn deploy src/token.lua  # deploy to testnet
\`\`\`

## Architecture

- **src/token.lua** — Token handler (mint, transfer, balance)
- **src/registry.lua** — Token registry for discoverability
- **src/counter.lua** — Simple counter example
- **frontend/** — React SPA with ArConnect wallet integration
- **test/** — Unit and integration tests

## Testing

\`\`\`bash
yarn test                          # all unit tests
yarn test test/aos.test.js         # AOS integration
yarn test test/hyperbeam.test.js   # HyperBEAM integration
\`\`\`

## Deployment

\`\`\`bash
yarn deploy src/token.lua                    # AO testnet
yarn deploy --local-hb src/token.lua         # local HyperBEAM
yarn deploy --mainnet src/token.lua          # production
\`\`\`
`,
  "tasks.json": JSON.stringify(DEMO_DATA, null, 2),
  "frontend/e2e/debug.spec.js": "",
  "frontend/e2e/token-transfer.spec.js": `import { test, expect } from "@playwright/test"

test.describe("Token Transfer E2E", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("http://localhost:5173")
  })

  test("should display app title", async ({ page }) => {
    await expect(page.locator("h1")).toHaveText("Token Transfer")
  })

  test("should show connect wallet prompt", async ({ page }) => {
    await expect(page.locator("text=Connecting to ArConnect")).toBeVisible()
  })

  test("should display balance after connection", async ({ page }) => {
    // Mock ArConnect wallet
    await page.evaluate(() => {
      window.arweaveWallet = {
        connect: async () => {},
        getActiveAddress: async () => "test-addr-123",
        sign: async (tx) => tx,
      }
    })
    await page.reload()
    await expect(page.locator("[data-testid=balance]")).toBeVisible({ timeout: 10000 })
  })

  test("should submit transfer form", async ({ page }) => {
    await page.evaluate(() => {
      window.arweaveWallet = {
        connect: async () => {},
        getActiveAddress: async () => "test-addr-123",
        sign: async (tx) => tx,
      }
    })
    await page.reload()
    await page.fill("[data-testid=recipient]", "recipient-addr-456")
    await page.fill("[data-testid=amount]", "100")
    await page.click("[data-testid=transfer-btn]")
    await expect(page.locator("[data-testid=status]")).toContainText("Transfer")
  })
})`,
  "frontend/src/App.css": `* { margin: 0; padding: 0; box-sizing: border-box; }

.app {
  max-width: 640px;
  margin: 0 auto;
  padding: 2rem;
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
}

h1 { font-size: 1.5rem; margin-bottom: 1.5rem; }

.card {
  border: 1px solid #e1e4e8;
  border-radius: 8px;
  padding: 1rem;
  margin-bottom: 1rem;
}

.balance { font-size: 2rem; font-weight: 600; }
.balance-label { color: #586069; font-size: 0.875rem; }

.form-group { margin-bottom: 1rem; }
.form-group label { display: block; margin-bottom: 0.25rem; font-size: 0.875rem; color: #586069; }
.form-group input { width: 100%; padding: 0.5rem; border: 1px solid #e1e4e8; border-radius: 6px; font-size: 1rem; }

.btn { padding: 0.5rem 1rem; border-radius: 6px; border: none; cursor: pointer; font-size: 1rem; font-weight: 500; }
.btn-primary { background: #2ea44f; color: white; }
.btn-primary:hover { background: #2c974b; }
.btn-primary:disabled { opacity: 0.6; cursor: not-allowed; }

.status { padding: 0.75rem; border-radius: 6px; margin-top: 1rem; font-size: 0.875rem; }
.status-success { background: #dcffe4; color: #22863a; }
.status-error { background: #ffeef0; color: #cb2431; }`,
  "frontend/src/App.jsx": `import { useState, useEffect } from "react"
import { AO } from "wao/web"
import TransferForm from "./components/TransferForm"
import BalanceDisplay from "./components/BalanceDisplay"

export default function App() {
  const [ao, setAo] = useState(null)
  const [connected, setConnected] = useState(false)

  useEffect(() => {
    const init = async () => {
      const ao = new AO()
      await ao.init()
      setAo(ao)
      setConnected(true)
    }
    init().catch(console.error)
  }, [])

  return (
    <div className="app">
      <h1>Token Transfer</h1>
      {connected ? (
        <>
          <BalanceDisplay ao={ao} />
          <TransferForm ao={ao} />
        </>
      ) : (
        <p>Connecting to ArConnect...</p>
      )}
    </div>
  )
}`,
  "frontend/src/index.css": `body {
  margin: 0;
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Oxygen,
    Ubuntu, Cantarell, "Fira Sans", "Droid Sans", "Helvetica Neue", sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

code {
  font-family: source-code-pro, Menlo, Monaco, Consolas, "Courier New", monospace;
}`,
  "frontend/src/main.jsx": `import React from "react"
import ReactDOM from "react-dom/client"
import App from "./App"
import "./index.css"

ReactDOM.createRoot(document.getElementById("root")).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
)`,
  "frontend/src/test-setup.js": "",
  "frontend/src/__tests__/App.test.jsx": `import { describe, it, expect } from "vitest"
import { render, screen, waitFor } from "@testing-library/react"
import App from "../App"

describe("App", () => {
  it("renders title", () => {
    render(<App />)
    expect(screen.getByText("Token Transfer")).toBeInTheDocument()
  })

  it("shows connecting message initially", () => {
    render(<App />)
    expect(screen.getByText("Connecting to ArConnect...")).toBeInTheDocument()
  })

  it("renders balance after connection", async () => {
    window.arweaveWallet = {
      connect: async () => {},
      getActiveAddress: async () => "test-addr",
    }
    render(<App />)
    await waitFor(() => {
      expect(screen.queryByText("Connecting")).not.toBeInTheDocument()
    })
  })
})`,
  "frontend/src/__tests__/TransferForm.test.jsx": `import { describe, it, expect, vi } from "vitest"
import { render, screen, fireEvent, waitFor } from "@testing-library/react"
import TransferForm from "../components/TransferForm"

const mockAo = {
  p: (pid) => ({
    m: vi.fn().mockResolvedValue({ out: "Transfer successful" })
  })
}

describe("TransferForm", () => {
  it("renders form fields", () => {
    render(<TransferForm ao={mockAo} />)
    expect(screen.getByLabelText(/recipient/i)).toBeInTheDocument()
    expect(screen.getByLabelText(/amount/i)).toBeInTheDocument()
  })

  it("validates empty fields", async () => {
    render(<TransferForm ao={mockAo} />)
    fireEvent.click(screen.getByRole("button", { name: /transfer/i }))
    await waitFor(() => {
      expect(screen.getByText(/required/i)).toBeInTheDocument()
    })
  })

  it("validates negative amounts", async () => {
    render(<TransferForm ao={mockAo} />)
    fireEvent.change(screen.getByLabelText(/amount/i), { target: { value: "-10" } })
    fireEvent.click(screen.getByRole("button", { name: /transfer/i }))
    await waitFor(() => {
      expect(screen.getByText(/positive/i)).toBeInTheDocument()
    })
  })

  it("submits transfer successfully", async () => {
    render(<TransferForm ao={mockAo} />)
    fireEvent.change(screen.getByLabelText(/recipient/i), { target: { value: "addr-123" } })
    fireEvent.change(screen.getByLabelText(/amount/i), { target: { value: "50" } })
    fireEvent.click(screen.getByRole("button", { name: /transfer/i }))
    await waitFor(() => {
      expect(screen.getByText(/successful/i)).toBeInTheDocument()
    })
  })
})`,
  "frontend/src/components/BalanceDisplay.jsx": `import { useState, useEffect } from "react"

export default function BalanceDisplay({ ao }) {
  const [balance, setBalance] = useState(null)

  useEffect(() => {
    if (!ao) return
    const fetchBalance = async () => {
      try {
        const { out } = await ao.p(import.meta.env.VITE_PROCESS_ID).m("Balance")
        setBalance(out)
      } catch (err) {
        console.error("Failed to fetch balance:", err)
      }
    }
    fetchBalance()
    const id = setInterval(fetchBalance, 10000)
    return () => clearInterval(id)
  }, [ao])

  return (
    <div className="card" data-testid="balance">
      <div className="balance-label">Your Balance</div>
      <div className="balance">{balance ?? "..."} TKN</div>
    </div>
  )
}`,
  "frontend/src/components/TokenInfo.jsx": `import { useState, useEffect } from "react"

export default function TokenInfo({ ao }) {
  const [info, setInfo] = useState(null)

  useEffect(() => {
    if (!ao) return
    const fetch = async () => {
      const { out } = await ao.p(import.meta.env.VITE_PROCESS_ID).m("Info")
      setInfo(JSON.parse(out))
    }
    fetch().catch(console.error)
  }, [ao])

  if (!info) return <div className="card">Loading token info...</div>

  return (
    <div className="card">
      <h3>{info.Name} ({info.Ticker})</h3>
      <p>Denomination: {info.Denomination}</p>
    </div>
  )
}`,
  "frontend/src/components/TransactionStatus.jsx": `export default function TransactionStatus({ status, error }) {
  if (!status && !error) return null
  return (
    <div className={\`status \${error ? "status-error" : "status-success"}\`} data-testid="status">
      {error || status}
    </div>
  )
}`,
  "frontend/src/components/TransferForm.jsx": "",
  "frontend/src/components/WalletConnect.jsx": `import { useState } from "react"

export default function WalletConnect({ onConnect }) {
  const [connecting, setConnecting] = useState(false)

  const handleConnect = async () => {
    setConnecting(true)
    try {
      await window.arweaveWallet.connect(["ACCESS_ADDRESS", "SIGN_TRANSACTION"])
      const addr = await window.arweaveWallet.getActiveAddress()
      onConnect(addr)
    } catch (err) {
      console.error("Wallet connection failed:", err)
    } finally {
      setConnecting(false)
    }
  }

  return (
    <button className="btn btn-primary" onClick={handleConnect} disabled={connecting}>
      {connecting ? "Connecting..." : "Connect Wallet"}
    </button>
  )
}`,
  "frontend/src/hooks/useToken.js": `import { useState, useEffect, useCallback } from "react"

export function useToken(ao, processId) {
  const [balance, setBalance] = useState(null)
  const [info, setInfo] = useState(null)
  const [loading, setLoading] = useState(false)

  const fetchBalance = useCallback(async () => {
    if (!ao || !processId) return
    try {
      const { out } = await ao.p(processId).m("Balance")
      setBalance(out)
    } catch (err) {
      console.error("Balance fetch failed:", err)
    }
  }, [ao, processId])

  const fetchInfo = useCallback(async () => {
    if (!ao || !processId) return
    try {
      const { out } = await ao.p(processId).m("Info")
      setInfo(JSON.parse(out))
    } catch (err) {
      console.error("Info fetch failed:", err)
    }
  }, [ao, processId])

  const transfer = useCallback(async (recipient, quantity) => {
    if (!ao || !processId) throw new Error("Not connected")
    setLoading(true)
    try {
      const { out } = await ao.p(processId).m("Transfer", {
        Recipient: recipient,
        Quantity: String(quantity),
      })
      await fetchBalance()
      return out
    } finally {
      setLoading(false)
    }
  }, [ao, processId, fetchBalance])

  useEffect(() => {
    fetchBalance()
    fetchInfo()
  }, [fetchBalance, fetchInfo])

  return { balance, info, loading, transfer, refresh: fetchBalance }
}`,
  "frontend/src/hooks/useWallet.js": `import { useState, useEffect } from "react"

export function useWallet() {
  const [address, setAddress] = useState(null)
  const [connected, setConnected] = useState(false)

  useEffect(() => {
    const checkConnection = async () => {
      try {
        if (window.arweaveWallet) {
          const addr = await window.arweaveWallet.getActiveAddress()
          setAddress(addr)
          setConnected(true)
        }
      } catch {}
    }
    checkConnection()
    window.addEventListener("arweaveWalletLoaded", checkConnection)
    return () => window.removeEventListener("arweaveWalletLoaded", checkConnection)
  }, [])

  const connect = async () => {
    await window.arweaveWallet.connect(["ACCESS_ADDRESS", "SIGN_TRANSACTION"])
    const addr = await window.arweaveWallet.getActiveAddress()
    setAddress(addr)
    setConnected(true)
  }

  const disconnect = () => {
    setAddress(null)
    setConnected(false)
  }

  return { address, connected, connect, disconnect }
}`,
  "HyperBEAM/src/dev_token.erl": `%%% @doc Token device for HyperBEAM.
%%% Manages token balances with mint, transfer, and balance queries.
%%% Tests are inline (HyperBEAM convention — device + eunit in same file).
-module(dev_token).
-export([info/0, init/3, execute/3]).
-include("include/hb.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

info() ->
    #{
        name => <<"Token">>,
        description => <<"Token transfer device">>,
        version => <<"1.0.0">>
    }.

init(_ID, _Params, State) ->
    {ok, State#{balances => #{}}}.

execute(<<"Mint">>, Msg, State = #{balances := Bals}) ->
    Sender = hb_message:get(<<"From">>, Msg),
    Qty = binary_to_integer(hb_message:get(<<"Quantity">>, Msg)),
    Current = maps:get(Sender, Bals, 0),
    NewBals = maps:put(Sender, Current + Qty, Bals),
    {ok, #{data => <<"Minted">>}, State#{balances => NewBals}};

execute(<<"Transfer">>, Msg, State = #{balances := Bals}) ->
    Sender = hb_message:get(<<"From">>, Msg),
    Recipient = hb_message:get(<<"Recipient">>, Msg),
    Qty = binary_to_integer(hb_message:get(<<"Quantity">>, Msg)),
    SenderBal = maps:get(Sender, Bals, 0),
    case SenderBal >= Qty of
        true ->
            NewBals = maps:put(Sender, SenderBal - Qty,
                maps:put(Recipient, maps:get(Recipient, Bals, 0) + Qty, Bals)),
            {ok, #{data => <<"Transferred">>}, State#{balances => NewBals}};
        false ->
            {error, <<"Insufficient balance">>}
    end;

execute(<<"Balance">>, Msg, State = #{balances := Bals}) ->
    Target = hb_message:get(<<"Target">>, Msg, hb_message:get(<<"From">>, Msg)),
    Bal = maps:get(Target, Bals, 0),
    {ok, #{data => integer_to_binary(Bal)}, State};

execute(_Action, _Msg, State) ->
    {ok, #{data => <<"Unknown action">>}, State}.

%%%===================================================================
%%% EUnit Tests
%%%===================================================================
-ifdef(TEST).

mint_test() ->
    {ok, State0} = init(<<"test">>, #{}, #{}),
    Msg = #{<<"From">> => <<"alice">>, <<"Quantity">> => <<"1000">>},
    {ok, #{data := <<"Minted">>}, State1} = execute(<<"Mint">>, Msg, State0),
    BalMsg = #{<<"From">> => <<"alice">>},
    {ok, #{data := <<"1000">>}, _} = execute(<<"Balance">>, BalMsg, State1).

transfer_test() ->
    {ok, S0} = init(<<"test">>, #{}, #{}),
    {ok, _, S1} = execute(<<"Mint">>,
        #{<<"From">> => <<"alice">>, <<"Quantity">> => <<"500">>}, S0),
    {ok, #{data := <<"Transferred">>}, S2} = execute(<<"Transfer">>,
        #{<<"From">> => <<"alice">>, <<"Recipient">> => <<"bob">>,
          <<"Quantity">> => <<"200">>}, S1),
    {ok, #{data := <<"300">>}, _} = execute(<<"Balance">>,
        #{<<"From">> => <<"alice">>}, S2),
    {ok, #{data := <<"200">>}, _} = execute(<<"Balance">>,
        #{<<"From">> => <<"bob">>}, S2).

insufficient_balance_test() ->
    {ok, S0} = init(<<"test">>, #{}, #{}),
    {ok, _, S1} = execute(<<"Mint">>,
        #{<<"From">> => <<"alice">>, <<"Quantity">> => <<"100">>}, S0),
    {error, <<"Insufficient balance">>} = execute(<<"Transfer">>,
        #{<<"From">> => <<"alice">>, <<"Recipient">> => <<"bob">>,
          <<"Quantity">> => <<"200">>}, S1).

-endif.`,

  "scripts/deploy.js": `import { readFileSync } from "node:fs"
import { AO } from "wao"

const src = process.argv[2]
if (!src) { console.error("Usage: yarn deploy <file.lua>"); process.exit(1) }

const jwk = JSON.parse(readFileSync(".wallet.json", "utf8"))
const ao = await new AO().init(jwk)
const src_data = readFileSync(src, "utf8")
const { pid } = await ao.deploy({ src_data })
console.log("Deployed:", pid)`,
  "src/counter.lua": `-- Simple Counter for AOS
Count = Count or 0

Handlers.add("increment",
  Handlers.utils.hasMatchingTag("Action", "Increment"),
  function(msg)
    Count = Count + 1
    msg.reply({ Data = tostring(Count) })
  end
)

Handlers.add("get",
  Handlers.utils.hasMatchingTag("Action", "Get"),
  function(msg)
    msg.reply({ Data = tostring(Count) })
  end
)`,
  "src/registry.lua": `-- Token Registry for AOS
local json = require("json")

Registry = Registry or {}

Handlers.add("register",
  Handlers.utils.hasMatchingTag("Action", "Register"),
  function(msg)
    local id = msg.Tags.ProcessId
    local name = msg.Tags.Name or "Unknown"
    Registry[id] = { name = name, owner = msg.From }
    msg.reply({ Data = "Registered: " .. name })
  end
)

Handlers.add("list",
  Handlers.utils.hasMatchingTag("Action", "List"),
  function(msg)
    msg.reply({ Data = json.encode(Registry) })
  end
)`,
  "src/token.lua": `-- Token Handler for AOS
local json = require("json")

Balances = Balances or {}
Name = Name or "Token"
Ticker = Ticker or "TKN"
Denomination = Denomination or 12

Handlers.add("info",
  Handlers.utils.hasMatchingTag("Action", "Info"),
  function(msg)
    msg.reply({
      Data = json.encode({ Name = Name, Ticker = Ticker, Denomination = Denomination })
    })
  end
)

Handlers.add("balance",
  Handlers.utils.hasMatchingTag("Action", "Balance"),
  function(msg)
    local target = msg.Tags.Target or msg.From
    local bal = Balances[target] or "0"
    msg.reply({ Data = bal, Tags = { Balance = bal, Target = target } })
  end
)

Handlers.add("transfer",
  Handlers.utils.hasMatchingTag("Action", "Transfer"),
  function(msg)
    local qty = tonumber(msg.Tags.Quantity)
    local from = msg.From
    local to = msg.Tags.Recipient
    assert(qty > 0, "Quantity must be positive")
    assert(tonumber(Balances[from] or "0") >= qty, "Insufficient balance")
    Balances[from] = tostring(tonumber(Balances[from]) - qty)
    Balances[to] = tostring(tonumber(Balances[to] or "0") + qty)
    msg.reply({ Data = "Transfer successful" })
    ao.send({ Target = to, Action = "Credit-Notice", Quantity = tostring(qty), Sender = from })
  end
)`,
  "test/aos.test.js": `import { describe, it } from "node:test"
import assert from "node:assert"
import { readFileSync } from "node:fs"
import { AO, acc } from "wao/test"

describe("AOS Integration", () => {
  it("should deploy and interact with counter", async () => {
    const ao = await new AO().init(acc[0])
    const src = readFileSync("src/counter.lua", "utf8")
    const { p } = await ao.deploy({ src_data: src })
    await p.m("Increment")
    await p.m("Increment")
    const { out } = await p.m("Get")
    assert.equal(out, "2")
  })
})`,
  "test/hyperbeam-token.test.js": `import { describe, it } from "node:test"
import assert from "node:assert"
import { readFileSync } from "node:fs"
import { AO, HyperBEAM, acc } from "wao/test"

describe("HyperBEAM Token Integration", () => {
  let hbeam, ao, p

  it("should start HyperBEAM", async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    ao = await new AO({ hb: hbeam.url }).init(acc[0])
  })

  it("should deploy token to HyperBEAM", async () => {
    const src = readFileSync("src/token.lua", "utf8")
    const result = await ao.deploy({ src_data: src })
    p = result.p
    assert.ok(p)
  })

  it("should mint tokens via HyperBEAM", async () => {
    await p.m("Mint", { Quantity: "5000" })
    const { out } = await p.m("Balance")
    assert.equal(out, "5000")
  })

  it("should transfer via HyperBEAM", async () => {
    await p.m("Transfer", { Recipient: acc[1].addr, Quantity: "200" })
    const { out } = await p.m("Balance")
    assert.equal(out, "4800")
  })

  it("should cleanup", () => hbeam?.kill())
})`,
  "test/hyperbeam.test.js": `import { describe, it } from "node:test"
import assert from "node:assert"
import { readFileSync } from "node:fs"
import { AO, HB, HyperBEAM, acc } from "wao/test"

describe("HyperBEAM", () => {
  let hbeam

  it("should start HyperBEAM node", async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    assert.ok(hbeam.port)
    assert.ok(hbeam.url)
  })

  it("should respond to info endpoint", async () => {
    const hb = new HB({ url: hbeam.url })
    const info = await hb.info()
    assert.ok(info.address)
  })

  it("should deploy and run AOS process", async () => {
    const ao = await new AO({ hb: hbeam.url }).init(acc[0])
    const src = readFileSync("src/counter.lua", "utf8")
    const { p } = await ao.deploy({ src_data: src })
    await p.m("Increment")
    const { out } = await p.m("Get")
    assert.equal(out, "1")
  })

  it("should cleanup", () => hbeam?.kill())
})`,
  "test/registry.test.js": `import { describe, it } from "node:test"
import assert from "node:assert"
import { readFileSync } from "node:fs"
import { AO, acc } from "wao/test"

describe("Registry", () => {
  let ao, p

  it("should deploy registry", async () => {
    ao = await new AO().init(acc[0])
    const src = readFileSync("src/registry.lua", "utf8")
    const result = await ao.deploy({ src_data: src })
    p = result.p
    assert.ok(p)
  })

  it("should register a token", async () => {
    const { out } = await p.m("Register", {
      ProcessId: "test-process-123",
      Name: "TestToken",
    })
    assert.match(out, /Registered/)
  })

  it("should list registered tokens", async () => {
    const { out } = await p.m("List")
    const registry = JSON.parse(out)
    assert.ok(registry["test-process-123"])
    assert.equal(registry["test-process-123"].name, "TestToken")
  })

  it("should register multiple tokens", async () => {
    await p.m("Register", { ProcessId: "proc-a", Name: "Alpha" })
    await p.m("Register", { ProcessId: "proc-b", Name: "Beta" })
    const { out } = await p.m("List")
    const registry = JSON.parse(out)
    assert.equal(Object.keys(registry).length, 3)
  })
})`,
  "test/token-device.test.js": `import { describe, it } from "node:test"
import assert from "node:assert"
import { HB, HyperBEAM, acc } from "wao/test"

describe("Token Device", () => {
  let hbeam, hb

  it("should start HyperBEAM with token device", async () => {
    hbeam = await new HyperBEAM({ reset: true }).ready()
    hb = new HB({ url: hbeam.url })
    assert.ok(hb)
  })

  it("should call token device info", async () => {
    const res = await hb.get({ path: "/~token@1.0/info" })
    assert.ok(res)
  })

  it("should cleanup", () => hbeam?.kill())
})`,
  "test/token.test.js": `import { describe, it } from "node:test"
import assert from "node:assert"
import { readFileSync } from "node:fs"
import { AO, acc } from "wao/test"

describe("Token", () => {
  let ao, p

  it("should deploy token process", async () => {
    ao = await new AO().init(acc[0])
    const src = readFileSync("src/token.lua", "utf8")
    const result = await ao.deploy({ src_data: src })
    p = result.p
    assert.ok(p)
  })

  it("should return token info", async () => {
    const { out } = await p.m("Info")
    const info = JSON.parse(out)
    assert.equal(info.Name, "Token")
  })

  it("should transfer tokens", async () => {
    await p.m("Mint", { Quantity: "1000" })
    await p.m("Transfer", { Recipient: acc[1].addr, Quantity: "100" })
    const { out } = await p.m("Balance")
    assert.equal(out, "900")
  })
})`,
}

// ═══════════════════════════════════════════════════════════════
// Constants
// ═══════════════════════════════════════════════════════════════

const TYPE_COLORS = {
  plan: "#8b5cf6", aos: "#3b82f6", "aos-test": "#14b8a6", "aos-integration": "#06b6d4",
  device: "#22c55e", "device-integration": "#059669",
  frontend: "#f97316", "frontend-test": "#ea580c", "frontend-integration": "#eab308",
  "module-lua": "#a855f7", "module-wasm": "#9333ea", "module-test": "#7c3aed",
  readme: "#6b7280", validate: "#84cc16",
}

const BADGE_COLORS = {
  orchestrator: "#16a34a", plan: "#8b5cf6", validate: "#ca8a04", generate: "#2563eb",
  build: "#ea580c", test: "#16a34a", info: "#6b7280", deploy: "#2563eb",
  scaffold: "#0d9488", debug: "#dc2626", dev: "#ea580c", team: "#7c3aed",
}

const FILE_BADGES = {
  json: { label: "JSON", color: "#3b82f6" }, md: { label: "MD", color: "#14b8a6" },
  css: { label: "CSS", color: "#8b5cf6" }, lua: { label: "Lua", color: "#3b82f6" },
  js: { label: "JS", color: "#eab308" }, jsx: { label: "JSX", color: "#eab308" },
  ts: { label: "TS", color: "#3b82f6" }, tsx: { label: "TSX", color: "#3b82f6" },
  erl: { label: "Erl", color: "#ef4444" }, rs: { label: "Rust", color: "#f97316" },
  toml: { label: "TOML", color: "#6b7280" }, html: { label: "HTML", color: "#f97316" },
  sh: { label: "Shell", color: "#22c55e" },
}

const EXT_TO_LANG = {
  js: "javascript", jsx: "javascript", ts: "typescript", tsx: "typescript",
  lua: "lua", json: "json", md: "markdown", css: "css", html: "xml",
  erl: "erlang", rs: "rust", sh: "bash", toml: "ini",
}

const SKILLS = {
  "Build Workflow": [
    { cmd: "/build", badge: "orchestrator", desc: "Full build workflow \u2014 plan, build, test, validate, README. Orchestrates all steps." },
    { cmd: "/plan", badge: "plan", desc: "Plan a feature \u2014 writes plan.md + tasks.json for persistent workflow." },
    { cmd: "/validate", badge: "validate", desc: "Post-build validation \u2014 tests, Lua pitfalls, handler coverage." },
    { cmd: "/readme", badge: "generate", desc: "Generate comprehensive README.md from plan, code, and tests." },
  ],
  "Build Steps": [
    { cmd: "/build-aos", badge: "build", desc: "Build AOS Lua scripts + in-memory tests, iterate until 100% pass." },
    { cmd: "/build-module", badge: "build", desc: "Build custom WASM64 (Rust) or standalone Lua modules + HyperBEAM integration tests." },
    { cmd: "/build-device", badge: "build", desc: "Build Erlang device + inline eunit tests, iterate until 100% pass." },
    { cmd: "/build-frontend", badge: "build", desc: "Build Vite + React components + vitest tests, iterate until 100% pass." },
  ],
  "Test Steps": [
    { cmd: "/test", badge: "test", desc: "Run in-memory AOS unit tests." },
    { cmd: "/test-hb", badge: "test", desc: "Run HyperBEAM integration tests with real Erlang node." },
    { cmd: "/test-device", badge: "test", desc: "WAO SDK integration tests for Erlang devices via HTTP." },
    { cmd: "/test-e2e", badge: "test", desc: "Playwright E2E tests with live HyperBEAM backend." },
  ],
  Utilities: [
    { cmd: "/report", badge: "info", desc: "Show progress on current plan \u2014 task status, test results." },
    { cmd: "/deploy", badge: "deploy", desc: "Deploy Lua source to testnet, local HB, or remote HB." },
    { cmd: "/create-aos", badge: "scaffold", desc: "Scaffold new AOS Lua script + test file." },
    { cmd: "/create-module", badge: "scaffold", desc: "Scaffold custom module (WASM64 Rust or standalone Lua) + test." },
    { cmd: "/create-device", badge: "scaffold", desc: "Scaffold new HyperBEAM Erlang device + test." },
    { cmd: "/debug", badge: "debug", desc: "Troubleshoot issues \u2014 port conflicts, WASM errors, compilation." },
    { cmd: "/dev", badge: "dev", desc: "Start Vite dev server for frontend development." },
    { cmd: "/team", badge: "team", desc: "Set up an agent team for parallel development." },
  ],
}

function deriveCommands(data) {
  const tasks = data?.tasks || []
  const luaFiles = []
  const testFiles = []
  const hbTestFiles = []
  const moduleTestFiles = []
  const frontendTestFiles = []
  const e2eFiles = []
  const hasDevice = tasks.some(t => t.type === "device")
  for (const t of tasks) {
    for (const f of (t.files || [])) {
      if (f.startsWith("src/") && f.endsWith(".lua")) luaFiles.push(f)
      if (f.startsWith("test/") && f.endsWith(".test.js")) {
        if (t.type === "aos-test") testFiles.push(f)
        else if (t.type === "aos-integration" || t.type === "device-integration") hbTestFiles.push(f)
        else if (t.type === "module-test") moduleTestFiles.push(f)
      }
      if (f.startsWith("frontend/") && f.endsWith(".test.jsx")) frontendTestFiles.push(f)
      if (f.startsWith("frontend/e2e/")) e2eFiles.push(f)
    }
  }
  const cmds = {}
  const testing = [{ cmd: "yarn test", desc: "Run all unit tests" }]
  for (const f of testFiles) testing.push({ cmd: `yarn test ${f}`, desc: `AOS unit tests \u2014 ${f.split("/").pop()}` })
  for (const f of moduleTestFiles) testing.push({ cmd: `yarn test ${f}`, desc: `Custom module tests \u2014 ${f.split("/").pop()}` })
  for (const f of hbTestFiles) testing.push({ cmd: `yarn test ${f}`, desc: `HyperBEAM integration \u2014 ${f.split("/").pop()}` })
  if (hasDevice) {
    const devFiles = tasks.filter(t => t.type === "device").flatMap(t => (t.files || []).filter(f => f.endsWith(".erl")))
    for (const f of devFiles) {
      const mod = f.split("/").pop().replace(".erl", "")
      testing.push({ cmd: `cd $HB_DIR && rebar3 eunit --module=${mod}`, desc: `Erlang eunit \u2014 ${mod}` })
    }
  }
  cmds["Testing"] = testing
  const deploy = []
  const luaArg = luaFiles.length === 1 ? ` ${luaFiles[0]}` : ""
  deploy.push({ cmd: `yarn deploy${luaArg}`, desc: `Deploy ${luaFiles.length > 1 ? "all src/*.lua" : luaArg.trim() || "scripts"} to AO testnet` })
  deploy.push({ cmd: `yarn deploy --local-hb${luaArg}`, desc: "Deploy to local HyperBEAM (genesis-wasm)" })
  deploy.push({ cmd: `yarn deploy --mainnet${luaArg}`, desc: "Deploy to remote HyperBEAM" })
  cmds["Deployment"] = deploy
  cmds["Development"] = [
    { cmd: "yarn start", desc: "Start dashboard (API :3333 + Vite :5174)" },
    { cmd: "yarn start:api", desc: "Start API server only (:3333)" },
    { cmd: "yarn keygen", desc: "Generate Arweave wallet (.wallet.json)" },
  ]
  const hasFrontend = tasks.some(t => t.type.startsWith("frontend"))
  if (hasFrontend) {
    const fe = [
      { cmd: "cd frontend && npm run dev", desc: "Start Vite dev server (port 5173)" },
    ]
    for (const f of frontendTestFiles) fe.push({ cmd: `cd frontend && npx vitest run ${f.replace("frontend/", "")}`, desc: `Vitest \u2014 ${f.split("/").pop()}` })
    if (frontendTestFiles.length === 0) fe.push({ cmd: "cd frontend && npm run test:unit", desc: "Run vitest component tests" })
    for (const f of e2eFiles) fe.push({ cmd: `cd frontend && npx playwright test ${f.replace("frontend/", "")}`, desc: `E2E \u2014 ${f.split("/").pop()}` })
    if (e2eFiles.length === 0) fe.push({ cmd: "cd frontend && npm run test:e2e", desc: "Run Playwright E2E tests" })
    fe.push({ cmd: "cd frontend && npm run build", desc: "Production build" })
    cmds["Frontend"] = fe
  }
  return cmds
}

const ENV_VARS = [
  { name: "PORT", desc: "HyperBEAM HTTP port", def: "10001" },
  { name: "MESSENGER_URL", desc: "Messenger unit URL", def: "(AO default)" },
  { name: "CU_URL", desc: "Compute unit URL", def: "(AO default)" },
  { name: "HB_URL", desc: "HyperBEAM URL", def: "http://localhost:10001" },
  { name: "WALLET_PATH", desc: "Arweave JWK path", def: ".wallet.json" },
]

// ═══════════════════════════════════════════════════════════════
// Helpers
// ═══════════════════════════════════════════════════════════════

function getTrackForType(type) {
  if (/^aos/.test(type)) return "AOS"
  if (/^device/.test(type)) return "Device"
  if (/^frontend/.test(type)) return "Frontend"
  if (/^module/.test(type)) return "Modules"
  if (/^(readme|validate)$/.test(type)) return "Validate"
  return null
}

function deriveTrackCards(tasks) {
  const tracks = {}
  for (const t of tasks) {
    const track = getTrackForType(t.type)
    if (!track) continue
    if (!tracks[track]) tracks[track] = { total: 0, done: 0, inProgress: 0 }
    tracks[track].total++
    if (t.status === "done") tracks[track].done++
    if (t.status === "in_progress") tracks[track].inProgress++
  }
  const order = ["AOS", "Modules", "Device", "Frontend", "Validate"]
  return order.filter(n => tracks[n]).map(name => {
    const t = tracks[name]
    const status = t.done === t.total ? "Done" : t.inProgress > 0 ? "In Progress" : "Pending"
    return { name, status }
  })
}

function formatDuration(ms) {
  if (ms < 0) return "0s"
  const s = Math.floor(ms / 1000) % 60
  const m = Math.floor(ms / 60000) % 60
  const h = Math.floor(ms / 3600000)
  if (h > 0) return `${h}h ${m}m`
  if (m > 0) return `${m}m ${s}s`
  return `${s}s`
}

function useElapsed(startedAt) {
  const [now, setNow] = useState(Date.now())
  useEffect(() => {
    if (!startedAt) return
    const id = setInterval(() => setNow(Date.now()), 1000)
    return () => clearInterval(id)
  }, [startedAt])
  if (!startedAt) return null
  return formatDuration(now - new Date(startedAt).getTime())
}

const formatSize = b => b < 1024 ? b + " B" : (b / 1024).toFixed(1) + " KB"
const getFileExt = p => { const d = p.lastIndexOf("."); return d > -1 ? p.slice(d + 1).toLowerCase() : "" }
const escapeHtml = t => t.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")

function groupFilesByDir(files) {
  const groups = {}
  for (const f of files) {
    const slash = f.path.lastIndexOf("/")
    const dir = slash > -1 ? f.path.slice(0, slash) + "/" : "{root}/"
    if (!groups[dir]) groups[dir] = []
    groups[dir].push(f)
  }
  return groups
}

// Build tree entries for current directory level
function getTreeEntries(files, currentPath) {
  const prefix = currentPath || ""
  const entries = []
  const seenDirs = new Set()

  for (const f of files) {
    const rel = prefix ? (f.path.startsWith(prefix) ? f.path.slice(prefix.length) : null) : f.path
    if (rel == null) continue
    const slashIdx = rel.indexOf("/")
    if (slashIdx > -1) {
      const dirName = rel.slice(0, slashIdx)
      if (!seenDirs.has(dirName)) {
        seenDirs.add(dirName)
        const dirPath = prefix + dirName + "/"
        const count = files.filter(ff => ff.path.startsWith(dirPath)).length
        entries.push({ type: "dir", name: dirName, path: dirPath, count })
      }
    } else {
      entries.push({ type: "file", name: rel, file: f })
    }
  }
  // Sort: dirs first, then files, alphabetical within each
  entries.sort((a, b) => {
    if (a.type !== b.type) return a.type === "dir" ? -1 : 1
    return a.name.localeCompare(b.name)
  })
  return entries
}

const FolderIcon = () => (
  <svg width="16" height="16" viewBox="0 0 16 16" fill="#54aeff">
    <path d="M1.75 1A1.75 1.75 0 0 0 0 2.75v10.5C0 14.216.784 15 1.75 15h12.5A1.75 1.75 0 0 0 16 13.25v-8.5A1.75 1.75 0 0 0 14.25 3H7.5a.25.25 0 0 1-.2-.1l-.9-1.2C6.07 1.26 5.55 1 5 1Z" />
  </svg>
)

function renderInline(text) {
  const re = /\*\*(.+?)\*\*|`([^`]+)`|\[(.+?)\]\((.+?)\)/g
  const parts = []; let last = 0, m, k = 0
  while ((m = re.exec(text))) {
    if (m.index > last) parts.push(text.slice(last, m.index))
    if (m[1] != null) parts.push(<strong key={k++}>{m[1]}</strong>)
    else if (m[2] != null) parts.push(<code key={k++}>{m[2]}</code>)
    else if (m[3] != null) parts.push(<a key={k++} href={m[4]}>{m[3]}</a>)
    last = m.index + m[0].length
  }
  if (last < text.length) parts.push(text.slice(last))
  return parts.length === 1 && typeof parts[0] === "string" ? parts[0] : parts
}

// ═══════════════════════════════════════════════════════════════
// Icons (SVG)
// ═══════════════════════════════════════════════════════════════

const MoonIcon = () => (
  <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor">
    <path d="M14.53 10.53a7 7 0 0 1-9.058-9.058A7.003 7.003 0 0 0 8 15a7.002 7.002 0 0 0 6.53-4.47z"/>
  </svg>
)
const SunIcon = () => (
  <svg width="16" height="16" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
    <circle cx="8" cy="8" r="2.5" fill="currentColor" stroke="none"/>
    <line x1="8" y1="1" x2="8" y2="3"/><line x1="8" y1="13" x2="8" y2="15"/>
    <line x1="1" y1="8" x2="3" y2="8"/><line x1="13" y1="8" x2="15" y2="8"/>
    <line x1="3.05" y1="3.05" x2="4.46" y2="4.46"/><line x1="11.54" y1="11.54" x2="12.95" y2="12.95"/>
    <line x1="12.95" y1="3.05" x2="11.54" y2="4.46"/><line x1="4.46" y1="11.54" x2="3.05" y2="12.95"/>
  </svg>
)
const BackIcon = () => (
  <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor">
    <path d="M7.78 12.53a.75.75 0 0 1-1.06 0L2.47 8.28a.75.75 0 0 1 0-1.06l4.25-4.25a.751.751 0 0 1 1.042.018.751.751 0 0 1 .018 1.042L4.81 7h7.44a.75.75 0 0 1 0 1.5H4.81l2.97 2.97a.75.75 0 0 1 0 1.06Z"/>
  </svg>
)
const FileIcon = ({ size = 14 }) => (
  <svg className="flex-shrink-0 color-fg-muted" width={size} height={size} viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
    <path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"/><polyline points="14 2 14 8 20 8"/>
  </svg>
)
const ChevronIcon = ({ open }) => (
  <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor" className="flex-shrink-0 color-fg-muted" style={{ transform: open ? "rotate(180deg)" : "none", transition: "transform 0.15s" }}>
    <path d="M12.78 5.22a.749.749 0 0 1 0 1.06l-4.25 4.25a.749.749 0 0 1-1.06 0L3.22 6.28a.749.749 0 1 1 1.06-1.06L8 8.939l3.72-3.719a.749.749 0 0 1 1.06 0Z"/>
  </svg>
)
const CopyIcon = () => (
  <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor">
    <path d="M0 6.75C0 5.784.784 5 1.75 5h1.5a.75.75 0 0 1 0 1.5h-1.5a.25.25 0 0 0-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 0 0 .25-.25v-1.5a.75.75 0 0 1 1.5 0v1.5A1.75 1.75 0 0 1 9.25 16h-7.5A1.75 1.75 0 0 1 0 14.25Z"/>
    <path d="M5 1.75C5 .784 5.784 0 6.75 0h7.5C15.216 0 16 .784 16 1.75v7.5A1.75 1.75 0 0 1 14.25 11h-7.5A1.75 1.75 0 0 1 5 9.25Zm1.75-.25a.25.25 0 0 0-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 0 0 .25-.25v-7.5a.25.25 0 0 0-.25-.25Z"/>
  </svg>
)
const CheckIcon = () => (
  <svg width="16" height="16" viewBox="0 0 16 16" fill="var(--color-success-fg, #1a7f37)">
    <path d="M13.78 4.22a.75.75 0 0 1 0 1.06l-7.25 7.25a.75.75 0 0 1-1.06 0L2.22 9.28a.751.751 0 0 1 .018-1.042.751.751 0 0 1 1.042-.018L6 10.94l6.72-6.72a.75.75 0 0 1 1.06 0Z"/>
  </svg>
)

// ═══════════════════════════════════════════════════════════════
// Highlighted code block
// ═══════════════════════════════════════════════════════════════

function CodeBlock({ code, lang }) {
  const [copied, setCopied] = useState(false)
  let html = escapeHtml(code)
  if (window.hljs && lang) {
    try { html = window.hljs.highlight(code, { language: lang }).value } catch {}
  }
  const handleCopy = () => {
    navigator.clipboard.writeText(code).then(() => {
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    }).catch(() => {})
  }
  return (
    <div className="code-block-wrap">
      <button className="code-copy-btn" type="button" onClick={handleCopy} aria-label="Copy code">
        {copied ? <CheckIcon /> : <CopyIcon />}
      </button>
      <pre><code className={lang ? `language-${lang} hljs` : ""} dangerouslySetInnerHTML={{ __html: html }} /></pre>
    </div>
  )
}

// ═══════════════════════════════════════════════════════════════
// Layout components
// ═══════════════════════════════════════════════════════════════

function Header({ dark, setDark, connected }) {
  return (
    <header className="Header" style={{ paddingLeft: 0, paddingRight: 0 }}>
      <div className="d-flex flex-items-center width-full px-4" style={{ maxWidth: 1012, margin: "0 auto", gap: 0 }}>
        <div className="Header-item mr-0">
          <img src="/favicon.png" alt="WAO" width="24" height="24" className="logo-invert" style={{ marginRight: 10 }} />
          <span className="Header-link f4 text-bold" style={{ color: "inherit", cursor: "default" }}>HyperADD</span>
              <span className="color-fg-muted d-none d-md-inline" style={{ marginLeft: 4, fontSize: 14 }}>/</span>
              <span className="color-fg-muted d-none d-md-inline" style={{ marginLeft: 4, fontSize: 14 }}>Agent Driven Development for AO &amp; HyperBEAM</span>
        </div>
        <div className="Header-item Header-item--full" />
        <div className="Header-item">
          <span className={`d-flex flex-items-center f6 ${connected ? "color-fg-success" : "color-fg-muted"}`} style={{ gap: 6 }}>
            <span className="d-inline-block" style={{ width: 8, height: 8, borderRadius: "50%", backgroundColor: "currentColor" }} />
            {connected ? "live" : "offline"}
          </span>
        </div>
        <div className="Header-item mr-0">
          <button className="dark-toggle" type="button" onClick={() => setDark(!dark)} aria-label="Toggle dark mode">
            {dark ? <SunIcon /> : <MoonIcon />}
          </button>
        </div>
      </div>
    </header>
  )
}

// GitHub repo icon (octicon-repo)
function RepoIcon() {
  return (
    <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor" style={{ flexShrink: 0 }}>
      <path d="M2 2.5A2.5 2.5 0 0 1 4.5 0h8.75a.75.75 0 0 1 .75.75v12.5a.75.75 0 0 1-.75.75h-2.5a.75.75 0 0 1 0-1.5h1.75v-2h-8a1 1 0 0 0-.714 1.7.75.75 0 1 1-1.072 1.05A2.495 2.495 0 0 1 2 11.5Zm10.5-1h-8a1 1 0 0 0-1 1v6.708A2.486 2.486 0 0 1 4.5 9h8ZM5 12.25a.25.25 0 0 1 .25-.25h3.5a.25.25 0 0 1 .25.25v3.25a.25.25 0 0 1-.4.2l-1.45-1.087a.249.249 0 0 0-.3 0L5.4 15.7a.25.25 0 0 1-.4-.2Z" />
    </svg>
  )
}

function ProgressSection({ data }) {
  const tasks = data?.tasks || []
  const done = tasks.filter(t => t.status === "done").length
  const total = tasks.length
  const pct = total > 0 ? Math.round((done / total) * 100) : 0
  const current = tasks.find(t => t.status === "in_progress")
  const remaining = total - done

  if (!data?.feature) {
    return <div className="px-4 pt-4"><p className="color-fg-muted m-0">No build in progress. Run /build to start.</p></div>
  }

  return (
    <div className="px-4 pt-4">
      <div className="d-flex mb-2" style={{ gap: 8, alignItems: "center" }}>
        <span className="color-fg-muted d-flex" style={{ alignItems: "center" }}><RepoIcon /></span>
        <span className="f3 text-bold color-fg-default" style={{ lineHeight: 1 }}>{data.feature}</span>
        <span className={`Label ${done === total ? "Label--success" : "Label--attention"}`} style={{ fontSize: 12, fontWeight: 500 }}>{done === total ? "complete" : "building"}</span>
      </div>
      <div className="d-flex flex-items-center" style={{ gap: 8 }}>
        <span className="f6 color-fg-muted no-wrap">{done} / {total}</span>
        <div className="d-flex flex-1" style={{ gap: 2, height: 8 }}>
          {tasks.map(t => (
            <div key={t.id} style={{
              flex: 1,
              borderRadius: 2,
              backgroundColor: t.status === "done" ? "#8250df" : t.status === "in_progress" ? "#bf8700" : "#d0d7de",
            }} />
          ))}
        </div>
        <span className="f6 color-fg-muted no-wrap">{pct}%</span>
      </div>
      {current && (
        <p className="f6 color-fg-muted m-0 mt-2">
          <span className="anim-spin d-inline-block mr-1">&#x21bb;</span>
          {current.name} &middot; {done} done, {remaining} remaining
        </p>
      )}
      {!current && total > 0 && (
        <p className="f6 color-fg-muted m-0 mt-2">
          {done === total ? `All ${total} tasks complete` : `${done} done, ${remaining} remaining`}
        </p>
      )}
    </div>
  )
}

const TRACK_LABEL = {
  Done: "Label Label--success",
  "In Progress": "Label Label--attention",
  Pending: "Label Label--secondary",
}
const TRACK_COLORS = {
  Done: { light: { bg: "#dafbe1", border: "rgba(26,127,55,0.4)" }, dark: { bg: "rgba(35,134,54,0.15)", border: "rgba(46,160,67,0.4)" } },
  "In Progress": { light: { bg: "#fff8c5", border: "rgba(154,103,0,0.4)" }, dark: { bg: "rgba(187,128,9,0.15)", border: "rgba(187,128,9,0.4)" } },
  Pending: { light: { bg: "transparent", border: "#d0d7de" }, dark: { bg: "transparent", border: "#30363d" } },
}

function TrackCards({ data, dark }) {
  const cards = deriveTrackCards(data?.tasks || [])
  if (!cards.length) return null
  const mode = dark ? "dark" : "light"
  return (
    <div className="d-flex flex-wrap px-4 pt-3" style={{ gap: 8 }}>
      {cards.map(c => {
        const colors = TRACK_COLORS[c.status]?.[mode] || TRACK_COLORS.Pending[mode]
        return (
          <div key={c.name} className="d-flex flex-items-center flex-justify-between rounded-2 p-3" style={{ flex: "1 1 0%", minWidth: 140, border: `1px solid ${colors.border}`, background: colors.bg }}>
            <span className="f5 text-bold">{c.name}</span>
            <span className={TRACK_LABEL[c.status]}>{c.status}</span>
          </div>
        )
      })}
    </div>
  )
}

function TabBar({ active, setActive, taskCount }) {
  const tabs = [
    { id: "tasks", label: "Tasks", counter: taskCount, icon: <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor"><path d="M8 9.5a1.5 1.5 0 1 0 0-3 1.5 1.5 0 0 0 0 3Z" /><path d="M8 0a8 8 0 1 1 0 16A8 8 0 0 1 8 0ZM1.5 8a6.5 6.5 0 1 0 13 0 6.5 6.5 0 0 0-13 0Z" /></svg> },
    { id: "tests", label: "Tests", icon: <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor"><path d="M5.75 7.5a.75.75 0 0 1 .75.75v1.5a.75.75 0 0 1-1.5 0v-1.5a.75.75 0 0 1 .75-.75Zm5.25.75a.75.75 0 0 0-1.5 0v1.5a.75.75 0 0 0 1.5 0ZM8 7.5a.75.75 0 0 1 .75.75v3.5a.75.75 0 0 1-1.5 0v-3.5A.75.75 0 0 1 8 7.5Z" /><path d="M4.25 1h2.5a.75.75 0 0 1 0 1.5h-.19l1.658 3.316A5.508 5.508 0 0 1 13.5 11.25a5.5 5.5 0 1 1-11 0 5.508 5.508 0 0 1 5.282-5.434L9.44 2.5h-.19a.75.75 0 0 1 0-1.5h2.5a.75.75 0 0 1 0 1.5h-.19L9.623 5.87A5.45 5.45 0 0 1 12 11.25a4 4 0 1 0-8 0 5.45 5.45 0 0 1 2.377-5.38L4.44 2.5h-.19a.75.75 0 0 1 0-1.5ZM8 7.25a4 4 0 1 0 0 8 4 4 0 0 0 0-8Z" /></svg> },
    { id: "plan", label: "Plan", icon: <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor"><path d="M0 1.75C0 .784.784 0 1.75 0h12.5C15.216 0 16 .784 16 1.75v9.5A1.75 1.75 0 0 1 14.25 13H8.06l-2.573 2.573A1.458 1.458 0 0 1 3 14.543V13H1.75A1.75 1.75 0 0 1 0 11.25Zm1.75-.25a.25.25 0 0 0-.25.25v9.5c0 .138.112.25.25.25h2a.75.75 0 0 1 .75.75v2.19l2.72-2.72a.749.749 0 0 1 .53-.22h6.5a.25.25 0 0 0 .25-.25v-9.5a.25.25 0 0 0-.25-.25Z" /></svg> },
    { id: "code", label: "Code", icon: <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor"><path d="m11.28 3.22 4.25 4.25a.75.75 0 0 1 0 1.06l-4.25 4.25a.749.749 0 0 1-1.275-.326.749.749 0 0 1 .215-.734L13.94 8l-3.72-3.72a.749.749 0 0 1 .326-1.275.749.749 0 0 1 .734.215Zm-6.56 0a.751.751 0 0 1 1.042.018.751.751 0 0 1 .018 1.042L2.06 8l3.72 3.72a.749.749 0 0 1-.326 1.275.749.749 0 0 1-.734-.215L.47 8.53a.75.75 0 0 1 0-1.06Z" /></svg> },
    { id: "readme", label: "README", icon: <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor"><path d="M0 1.75A.75.75 0 0 1 .75 1h4.253c1.227 0 2.317.59 3 1.501A3.743 3.743 0 0 1 11.006 1h4.245a.75.75 0 0 1 .75.75v10.5a.75.75 0 0 1-.75.75h-4.507a2.25 2.25 0 0 0-1.591.659l-.622.621a.75.75 0 0 1-1.06 0l-.622-.621A2.25 2.25 0 0 0 5.258 13H.75a.75.75 0 0 1-.75-.75Zm7.251 10.324.004-5.073-.002-2.253A2.25 2.25 0 0 0 5.003 2.5H1.5v9h3.757a3.75 3.75 0 0 1 1.994.574ZM8.755 4.75l-.004 7.322a3.752 3.752 0 0 1 1.992-.572H14.5v-9h-3.495a2.25 2.25 0 0 0-2.25 2.25Z" /></svg> },
    { id: "commands", label: "Commands", icon: <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor"><path d="M0 2.75C0 1.784.784 1 1.75 1h12.5c.966 0 1.75.784 1.75 1.75v10.5A1.75 1.75 0 0 1 14.25 15H1.75A1.75 1.75 0 0 1 0 13.25Zm1.75-.25a.25.25 0 0 0-.25.25v10.5c0 .138.112.25.25.25h12.5a.25.25 0 0 0 .25-.25V2.75a.25.25 0 0 0-.25-.25Zm7.25 8a.75.75 0 0 1 .75-.75h1.5a.75.75 0 0 1 0 1.5h-1.5a.75.75 0 0 1-.75-.75Zm-7.25-6a.75.75 0 0 1 .75-.75h2a.75.75 0 0 1 0 1.5h-2A.75.75 0 0 1 1.75 4.5ZM4.22 6.22a.75.75 0 0 1 1.06 0l2 2a.75.75 0 0 1 0 1.06l-2 2a.749.749 0 0 1-1.275-.326.749.749 0 0 1 .215-.734L5.94 8.5 4.22 6.78a.75.75 0 0 1 0-1.06Z" /></svg> },
    { id: "skills", label: "Skills", icon: <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor"><path d="M7.998 14.5c2.832 0 5-1.98 5-4.5 0-1.463-.68-2.19-1.879-3.383l-.036-.037c-1.013-1.008-2.3-2.29-2.834-4.434-.322.256-.63.579-.864.953-.432.696-.621 1.58-.046 2.73.473.947.67 2.284-.278 3.232-.61.61-1.545.84-2.403.508a2.18 2.18 0 0 1-.727-.467 2.2 2.2 0 0 1-.61-1.15c-.208.208-.401.397-.571.566C1.558 9.882 1 10.56 1 11.994 1 14.476 3.842 16 7.999 16c4.157 0 6.999-1.524 6.999-4.006 0-1.434-.558-2.112-1.749-3.298l-.037-.036c-1.175-1.168-2.665-2.646-3.28-5.348a.96.96 0 0 0-.109-.218.756.756 0 0 0-1.283.065c-.571.922-1.378 2.016-2.498 2.772-.484.326-1.037.504-1.592.504-.345 0-.69-.072-1.016-.22a2.52 2.52 0 0 1-.657-.428l-.122-.126c-.276.28-.543.574-.786.894A6.11 6.11 0 0 0 1 11.994c0 2.483 2.842 4.006 6.999 4.006Z" /><path d="M3.635 10.326c.199.63.758 1.024 1.363 1.024.474 0 .943-.227 1.255-.611a.755.755 0 0 0-.008-1.003c-.327-.375-.327-.94 0-1.316a2.078 2.078 0 0 0 .417-1.987c-1.08.591-1.833 1.376-2.484 2.253a5.38 5.38 0 0 0-.543 1.64Z" /></svg> },
    { id: "deploy", label: "Deploy", icon: <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor"><path d="M8.75.75V2h.985c.304 0 .603.08.867.231l1.29.736c.038.022.08.033.124.033h2.234a.75.75 0 0 1 0 1.5h-.427l2.111 4.692a.75.75 0 0 1-.154.838l-.53-.53.529.531-.001.002-.002.002-.006.006-.006.005-.01.01a.753.753 0 0 1-.07.063 3.04 3.04 0 0 1-.39.276 3.555 3.555 0 0 1-1.862.497c-.786 0-1.4-.227-1.862-.497a3.04 3.04 0 0 1-.39-.276.749.749 0 0 1-.07-.063l-.01-.01-.006-.005-.004-.004-.004-.004a.75.75 0 0 1-.154-.838L13.823 4.5h-.427a1.681 1.681 0 0 1-.497-.078l-1.29-.736a.164.164 0 0 0-.072-.019H8.75v8.172a2.332 2.332 0 0 1 1.422 1.161H11.5a.75.75 0 0 1 0 1.5h-1.328a2.343 2.343 0 0 1-4.344 0H4.5a.75.75 0 0 1 0-1.5h1.328A2.332 2.332 0 0 1 7.25 11.84V3.667h-.985a.164.164 0 0 0-.072.019l-1.29.736a1.68 1.68 0 0 1-.497.078h-.427l2.111 4.692a.75.75 0 0 1-.154.838l-.53-.53.529.531-.001.002-.002.002-.006.006-.006.005-.01.01a.756.756 0 0 1-.07.063 3.04 3.04 0 0 1-.39.276 3.555 3.555 0 0 1-1.862.497c-.786 0-1.4-.227-1.862-.497a3.04 3.04 0 0 1-.39-.276.749.749 0 0 1-.07-.063l-.01-.01-.006-.005-.004-.004-.004-.004a.75.75 0 0 1-.154-.838L4.823 4.5H2.25a.75.75 0 0 1 0-1.5h2.234c.044 0 .086-.011.124-.033l1.29-.736A1.68 1.68 0 0 1 6.765 2H7.25V.75a.75.75 0 0 1 1.5 0Zm-4.384 8.892h.68l-.34-.754Zm6.588 0h.68l-.34-.754ZM8 13.5a.843.843 0 1 0 0-1.686.843.843 0 0 0 0 1.686Z" /></svg> },
  ]
  return (
    <nav className="UnderlineNav mx-4 mt-3">
      <div className="UnderlineNav-body">
        {tabs.map(t => (
          <button key={t.id} className="UnderlineNav-item" role="tab" type="button" aria-selected={active === t.id} onClick={() => setActive(t.id)}>
            <span className="d-flex flex-items-center" style={{ gap: 6 }}>
              <span className="d-flex color-fg-muted">{t.icon}</span>
              {t.label}
              {t.counter != null && <span className="Counter">{t.counter}</span>}
            </span>
          </button>
        ))}
      </div>
    </nav>
  )
}

// ═══════════════════════════════════════════════════════════════
// Tab: Tasks (GitHub Issues-style)
// ═══════════════════════════════════════════════════════════════

// GitHub-style issue state icons
const IssueOpenIcon = () => (
  <svg width="16" height="16" viewBox="0 0 16 16" fill="#1a7f37">
    <path d="M8 9.5a1.5 1.5 0 1 0 0-3 1.5 1.5 0 0 0 0 3Z"/>
    <path d="M8 0a8 8 0 1 1 0 16A8 8 0 0 1 8 0ZM1.5 8a6.5 6.5 0 1 0 13 0 6.5 6.5 0 0 0-13 0Z"/>
  </svg>
)
const IssueClosedIcon = () => (
  <svg width="16" height="16" viewBox="0 0 16 16" fill="#8250df">
    <path d="M11.28 6.78a.75.75 0 0 0-1.06-1.06L7.25 8.69 5.78 7.22a.75.75 0 0 0-1.06 1.06l2 2a.75.75 0 0 0 1.06 0l3.5-3.5Z"/>
    <path d="M16 8A8 8 0 1 1 0 8a8 8 0 0 1 16 0Zm-1.5 0a6.5 6.5 0 1 0-13 0 6.5 6.5 0 0 0 13 0Z"/>
  </svg>
)
const IssueInProgressIcon = () => (
  <svg width="16" height="16" viewBox="0 0 16 16" fill="#bf8700">
    <path d="M8 9.5a1.5 1.5 0 1 0 0-3 1.5 1.5 0 0 0 0 3Z"/>
    <path d="M8 0a8 8 0 1 1 0 16A8 8 0 0 1 8 0ZM1.5 8a6.5 6.5 0 1 0 13 0 6.5 6.5 0 0 0-13 0Z"/>
  </svg>
)

function TaskRow({ t, isOpen, onToggle }) {
  const isDone = t.status === "done"
  const isActive = t.status === "in_progress"
  const color = TYPE_COLORS[t.type] || "#6b7280"
  const elapsed = useElapsed(isActive ? t.started_at : null)
  const duration = isDone && t.started_at && t.completed_at
    ? formatDuration(new Date(t.completed_at).getTime() - new Date(t.started_at).getTime())
    : null

  const timeAgo = t.completed_at
    ? `completed ${formatDuration(Date.now() - new Date(t.completed_at).getTime())} ago`
    : t.started_at
    ? `started ${formatDuration(Date.now() - new Date(t.started_at).getTime())} ago`
    : null

  return (
    <>
      <div className="Box-row d-flex flex-items-start px-3 py-2" style={{ gap: 8, cursor: "pointer" }} onClick={onToggle}>
        <div className="flex-shrink-0" style={{ paddingTop: 3 }}>
          {isDone && <IssueClosedIcon />}
          {isActive && <IssueInProgressIcon />}
          {!isDone && !isActive && <IssueOpenIcon />}
        </div>
        <div className="flex-1" style={{ minWidth: 0 }}>
          <div className="d-flex flex-items-center flex-wrap" style={{ gap: 6 }}>
            <a className={`f4 text-bold ${isDone ? "color-fg-muted" : "color-fg-default"}`} style={{ cursor: "pointer", textDecoration: "none" }}>
              {t.name}
            </a>
            <span className="Label" style={{ color, borderColor: color + "44", backgroundColor: color + "18", fontSize: 12, verticalAlign: "middle" }}>{t.type}</span>
          </div>
          <div className="f6 color-fg-muted" style={{ marginTop: 4 }}>
            #{t.id}
            {t.skill && <> &middot; <code style={{ fontSize: 11 }}>{t.skill}</code></>}
            {isActive && elapsed && <> &middot; <span className="color-fg-attention"><span className="anim-spin d-inline-block mr-1" style={{ fontSize: 10 }}>&#x21bb;</span>{elapsed}</span></>}
            {duration && <> &middot; took {duration}</>}
            {timeAgo && <> &middot; {timeAgo}</>}
          </div>
        </div>
        <ChevronIcon open={isOpen} />
      </div>
      {isOpen && (
        <div className="color-bg-subtle border-bottom" style={{ padding: "12px 16px 12px 40px" }}>
          <table className="f6" style={{ lineHeight: "22px", wordBreak: "break-word" }}>
            <tbody>
              <tr>
                <td className="color-fg-muted pr-3 no-wrap v-align-top">Status</td>
                <td><span className={`Label ${isDone ? "Label--success" : isActive ? "Label--attention" : "Label--secondary"}`}>{t.status.replace("_", " ")}</span></td>
              </tr>
              {t.done_when && <tr>
                <td className="color-fg-muted pr-3 no-wrap v-align-top">Done when</td>
                <td>{t.done_when}</td>
              </tr>}
              {t.files?.length > 0 && <tr>
                <td className="color-fg-muted pr-3 no-wrap v-align-top">Files</td>
                <td>{t.files.map(f => <code key={f} className="f6 mr-1">{f}</code>)}</td>
              </tr>}
              {t.started_at && <tr>
                <td className="color-fg-muted pr-3 no-wrap v-align-top">Started</td>
                <td>{new Date(t.started_at).toLocaleTimeString()}</td>
              </tr>}
              {t.completed_at && <tr>
                <td className="color-fg-muted pr-3 no-wrap v-align-top">Completed</td>
                <td>{new Date(t.completed_at).toLocaleTimeString()} ({duration})</td>
              </tr>}
              {isActive && elapsed && <tr>
                <td className="color-fg-muted pr-3 no-wrap v-align-top">Elapsed</td>
                <td className="color-fg-attention text-bold">{elapsed}</td>
              </tr>}
            </tbody>
          </table>
        </div>
      )}
    </>
  )
}

// Group tasks by track (like GitHub Projects sections)
const TASK_GROUPS = [
  { key: "plan", label: "Planning", match: t => t.type === "plan" },
  { key: "aos", label: "AOS", match: t => t.type.startsWith("aos") },
  { key: "device", label: "Device", match: t => t.type.startsWith("device") },
  { key: "frontend", label: "Frontend", match: t => t.type.startsWith("frontend") },
  { key: "module", label: "Custom Module", match: t => t.type.startsWith("module") },
  { key: "validation", label: "Validation", match: t => ["readme", "validate", "deploy"].includes(t.type) },
]

function groupTasks(tasks) {
  const groups = []
  const used = new Set()
  for (const g of TASK_GROUPS) {
    const items = tasks.filter((t, i) => { if (used.has(i)) return false; if (g.match(t)) { used.add(i); return true } return false })
    if (items.length) groups.push({ ...g, tasks: items })
  }
  // Catch-all for any unmatched
  const rest = tasks.filter((_, i) => !used.has(i))
  if (rest.length) groups.push({ key: "other", label: "Other", tasks: rest })
  return groups
}

function TaskGroupHeader({ label, count, done, collapsed, onToggle }) {
  return (
    <div
      className="d-flex flex-items-center color-bg-subtle px-3 py-2 border-bottom"
      style={{ cursor: "pointer", userSelect: "none", gap: 8 }}
      onClick={onToggle}
    >
      <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor" className="color-fg-muted" style={{ transform: collapsed ? "rotate(-90deg)" : "rotate(0deg)", transition: "transform 0.15s" }}>
        <path d="M12.78 5.22a.749.749 0 0 1 0 1.06l-4.25 4.25a.749.749 0 0 1-1.06 0L3.22 6.28a.749.749 0 1 1 1.06-1.06L8 8.939l3.72-3.719a.749.749 0 0 1 1.06 0Z" />
      </svg>
      <span className="text-bold f5">{label}</span>
      <span className="Counter">{count}</span>
      <span className="f6 color-fg-muted">{done === count ? "Complete" : `${done} / ${count}`}</span>
    </div>
  )
}

function TasksTab({ data }) {
  const [expanded, setExpanded] = useState(null)
  const [collapsed, setCollapsed] = useState({})
  const tasks = data?.tasks || []
  if (!tasks.length) return <p className="p-4 color-fg-muted">No tasks yet. Run /build to create a task list.</p>

  const groups = groupTasks(tasks)

  return (
    <div className="Box mx-4 mt-3 mb-4">
      {groups.map(g => {
        const done = g.tasks.filter(t => t.status === "done").length
        const isCollapsed = collapsed[g.key]
        return (
          <div key={g.key}>
            <TaskGroupHeader
              label={g.label}
              count={g.tasks.length}
              done={done}
              collapsed={isCollapsed}
              onToggle={() => setCollapsed(prev => ({ ...prev, [g.key]: !prev[g.key] }))}
            />
            {!isCollapsed && g.tasks.map(t => (
              <TaskRow key={t.id} t={t} isOpen={expanded === t.id} onToggle={() => setExpanded(expanded === t.id ? null : t.id)} />
            ))}
          </div>
        )
      })}
    </div>
  )
}

// ═══════════════════════════════════════════════════════════════
// Tab: Tests
// ═══════════════════════════════════════════════════════════════

const DEMO_TESTS = [
  {
    file: "test/aos.test.js",
    group: "AOS",
    tests: [
      { name: "should deploy AOS process", status: "pass", duration: 742 },
      { name: "should increment counter", status: "pass", duration: 118 },
      { name: "should return count via dry-run", status: "pass", duration: 85 },
    ],
  },
  {
    file: "test/token.test.js",
    group: "AOS",
    tests: [
      { name: "should mint initial supply", status: "pass", duration: 856 },
      { name: "should transfer tokens between accounts", status: "pass", duration: 234 },
      { name: "should reject transfer exceeding balance", status: "pass", duration: 112 },
      { name: "should return correct balance", status: "pass", duration: 91 },
    ],
  },
  {
    file: "test/registry.test.js",
    group: "AOS",
    tests: [
      { name: "should register token", status: "pass", duration: 678 },
      { name: "should list registered tokens", status: "pass", duration: 145 },
      { name: "should reject duplicate registration", status: "pass", duration: 98 },
      { name: "should deregister token", status: "pass", duration: 134 },
      { name: "should query token metadata", status: "pass", duration: 107 },
    ],
  },
  {
    file: "HyperBEAM/src/dev_token.erl",
    group: "Device",
    tests: [
      { name: "mint_test", status: "pass", duration: 12 },
      { name: "transfer_test", status: "pass", duration: 15 },
      { name: "insufficient_balance_test", status: "pass", duration: 8 },
    ],
  },
  {
    file: "test/hyperbeam-token.test.js",
    group: "Device",
    tests: [
      { name: "should deploy token on HyperBEAM", status: "pass", duration: 2341 },
      { name: "should mint via HyperBEAM", status: "pass", duration: 1456 },
      { name: "should transfer via HyperBEAM", status: "pass", duration: 1289 },
    ],
  },
  {
    file: "test/token-device.test.js",
    group: "Device",
    tests: [
      { name: "should call device via HTTP", status: "pass", duration: 1823 },
      { name: "should return device state", status: "pass", duration: 945 },
    ],
  },
  {
    file: "test/hyperbeam.test.js",
    group: "Device",
    tests: [
      { name: "should start HyperBEAM node", status: "pass", duration: 3210 },
      { name: "should deploy AOS on HyperBEAM", status: "pass", duration: 1876 },
      { name: "should send message via slot", status: "pass", duration: 567 },
      { name: "should read state via dry-run", status: "pass", duration: 412 },
    ],
  },
  {
    file: "frontend/src/__tests__/App.test.jsx",
    group: "Frontend",
    tests: [
      { name: "renders app without crashing", status: "pass", duration: 45 },
      { name: "shows wallet connect button", status: "pass", duration: 32 },
      { name: "displays token info after connect", status: "pass", duration: 67 },
    ],
  },
  {
    file: "frontend/src/__tests__/TransferForm.test.jsx",
    group: "Frontend",
    tests: [
      { name: "renders transfer form", status: "pass", duration: 28 },
      { name: "validates recipient address", status: "pass", duration: 19 },
      { name: "validates amount field", status: "pass", duration: 21 },
      { name: "submits transfer", status: "pass", duration: 156 },
      { name: "shows error on failed transfer", status: "fail", duration: 203 },
    ],
  },
  {
    file: "frontend/e2e/token-transfer.spec.js",
    group: "Frontend",
    tests: [
      { name: "connects wallet and loads balance", status: "in_progress", duration: null },
      { name: "sends token transfer end-to-end", status: "pending", duration: null },
      { name: "shows transaction confirmation", status: "pending", duration: null },
    ],
  },
]

const TEST_GROUPS = [
  { key: "aos", label: "AOS", match: f => f.group === "AOS" },
  { key: "device", label: "Device", match: f => f.group === "Device" },
  { key: "frontend", label: "Frontend", match: f => f.group === "Frontend" },
]

function groupTestFiles(files) {
  const groups = []
  const used = new Set()
  for (const g of TEST_GROUPS) {
    const items = files.filter((f, i) => { if (used.has(i)) return false; if (g.match(f)) { used.add(i); return true } return false })
    if (items.length) groups.push({ ...g, files: items })
  }
  const rest = files.filter((_, i) => !used.has(i))
  if (rest.length) groups.push({ key: "other", label: "Other", files: rest })
  return groups
}

function TestFileRow({ file, isOpen, onToggle }) {
  const total = file.tests.length
  const passed = file.tests.filter(t => t.status === "pass").length
  const failed = file.tests.filter(t => t.status === "fail").length
  const running = file.tests.filter(t => t.status === "in_progress").length
  const allPass = passed === total
  const totalDuration = file.tests.reduce((s, t) => s + (t.duration || 0), 0)

  return (
    <>
      <div className="Box-row d-flex flex-items-center px-3 py-2" style={{ gap: 8, cursor: "pointer" }} onClick={onToggle}>
        <div className="flex-shrink-0" style={{ width: 22, display: "flex", justifyContent: "center" }}>
          {failed > 0 ? (
            <svg width="16" height="16" viewBox="0 0 16 16" fill="#cf222e"><path d="M2.343 13.657A8 8 0 1 1 13.658 2.343 8 8 0 0 1 2.343 13.657ZM6.03 4.97a.751.751 0 0 0-1.042.018.751.751 0 0 0-.018 1.042L6.94 8 4.97 9.97a.749.749 0 0 0 .326 1.275.749.749 0 0 0 .734-.215L8 9.06l1.97 1.97a.749.749 0 0 0 1.275-.326.749.749 0 0 0-.215-.734L9.06 8l1.97-1.97a.749.749 0 0 0-.326-1.275.749.749 0 0 0-.734.215L8 6.94Z" /></svg>
          ) : running > 0 ? (
            <span className="anim-spin d-inline-block color-fg-attention" style={{ fontSize: 14 }}>&#x21bb;</span>
          ) : allPass ? (
            <svg width="16" height="16" viewBox="0 0 16 16" fill="#1a7f37"><path d="M8 16A8 8 0 1 1 8 0a8 8 0 0 1 0 16Zm3.78-9.72a.751.751 0 0 0-.018-1.042.751.751 0 0 0-1.042-.018L6.75 9.19 5.28 7.72a.751.751 0 0 0-1.042.018.751.751 0 0 0-.018 1.042l2 2a.75.75 0 0 0 1.06 0Z" /></svg>
          ) : (
            <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor" className="color-fg-muted"><circle cx="8" cy="8" r="7" fill="none" stroke="currentColor" strokeWidth="2" /></svg>
          )}
        </div>
        <div className="flex-1" style={{ minWidth: 0 }}>
          <span className="f5 text-bold color-fg-default" style={{ fontFamily: "ui-monospace, SFMono-Regular, 'SF Mono', Menlo, Consolas, monospace", fontSize: 13 }}>{file.file}</span>
        </div>
        <span className="f6 color-fg-muted no-wrap">{totalDuration > 0 ? formatDuration(totalDuration) : ""}</span>
        <span className={`Label ${allPass ? "Label--success" : failed > 0 ? "Label--danger" : "Label--secondary"}`} style={{ fontSize: 12 }}>{passed}/{total}</span>
        <ChevronIcon open={isOpen} />
      </div>
      {isOpen && (
        <div className="color-bg-subtle border-bottom" style={{ padding: "4px 0" }}>
          {file.tests.map((t, i) => (
            <div key={i} className="d-flex flex-items-center px-3" style={{ gap: 8, padding: "4px 16px 4px 48px" }}>
              <div style={{ width: 16, display: "flex", justifyContent: "center", flexShrink: 0 }}>
                {t.status === "pass" && <svg width="12" height="12" viewBox="0 0 16 16" fill="#1a7f37"><path d="M13.78 4.22a.75.75 0 0 1 0 1.06l-7.25 7.25a.75.75 0 0 1-1.06 0L2.22 9.28a.751.751 0 0 1 .018-1.042.751.751 0 0 1 1.042-.018L6 10.94l6.72-6.72a.75.75 0 0 1 1.06 0Z" /></svg>}
                {t.status === "fail" && <svg width="12" height="12" viewBox="0 0 16 16" fill="#cf222e"><path d="M3.72 3.72a.75.75 0 0 1 1.06 0L8 6.94l3.22-3.22a.749.749 0 0 1 1.275.326.749.749 0 0 1-.215.734L9.06 8l3.22 3.22a.749.749 0 0 1-.326 1.275.749.749 0 0 1-.734-.215L8 9.06l-3.22 3.22a.751.751 0 0 1-1.042-.018.751.751 0 0 1-.018-1.042L6.94 8 3.72 4.78a.75.75 0 0 1 0-1.06Z" /></svg>}
                {t.status === "in_progress" && <span className="anim-spin d-inline-block color-fg-attention" style={{ fontSize: 10 }}>&#x21bb;</span>}
                {t.status === "pending" && <span className="d-inline-block" style={{ width: 8, height: 8, borderRadius: "50%", border: "2px solid var(--color-border-default)" }} />}
              </div>
              <span className={`f6 flex-1 ${t.status === "pass" ? "color-fg-muted" : t.status === "fail" ? "color-fg-danger" : "color-fg-default"}`}>{t.name}</span>
              {t.duration != null && <span className="f6 color-fg-muted no-wrap">{t.duration}ms</span>}
            </div>
          ))}
        </div>
      )}
    </>
  )
}

function TestGroupHeader({ label, count, passed, failed, collapsed, onToggle }) {
  return (
    <div
      className="d-flex flex-items-center color-bg-subtle px-3 py-2 border-bottom"
      style={{ cursor: "pointer", userSelect: "none", gap: 8 }}
      onClick={onToggle}
    >
      <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor" className="color-fg-muted" style={{ transform: collapsed ? "rotate(-90deg)" : "rotate(0deg)", transition: "transform 0.15s" }}>
        <path d="M12.78 5.22a.749.749 0 0 1 0 1.06l-4.25 4.25a.749.749 0 0 1-1.06 0L3.22 6.28a.749.749 0 1 1 1.06-1.06L8 8.939l3.72-3.719a.749.749 0 0 1 1.06 0Z" />
      </svg>
      <span className="text-bold f5">{label}</span>
      <span className="Counter">{count} tests</span>
      {failed > 0
        ? <span className="f6 color-fg-danger">{failed} failed</span>
        : <span className="f6 color-fg-muted">{passed === count ? "All passing" : `${passed} / ${count}`}</span>
      }
    </div>
  )
}

function TestsTab() {
  const [testData, setTestData] = useState(DEMO_TESTS)
  const [expanded, setExpanded] = useState(null)
  const [collapsed, setCollapsed] = useState({})

  useEffect(() => {
    fetch("/api/tests").then(r => r.json()).then(d => { if (d?.length) setTestData(d) }).catch(() => {})
  }, [])

  const groups = groupTestFiles(testData)
  const allTests = testData.flatMap(f => f.tests)
  const totalPass = allTests.filter(t => t.status === "pass").length
  const totalFail = allTests.filter(t => t.status === "fail").length
  const totalCount = allTests.length
  const pct = totalCount > 0 ? Math.round((totalPass / totalCount) * 100) : 0

  return (
    <div className="mx-4 mt-3 mb-4">
      <div className="d-flex flex-items-center mb-3 px-1" style={{ gap: 12 }}>
        <span className="f5 text-bold">{totalPass} / {totalCount} passing</span>
        <span className="f6 color-fg-muted">({pct}%)</span>
        {totalFail > 0 && <span className="Label Label--danger">{totalFail} failed</span>}
      </div>
      <div className="Box">
        {groups.map(g => {
          const tests = g.files.flatMap(f => f.tests)
          const passed = tests.filter(t => t.status === "pass").length
          const failed = tests.filter(t => t.status === "fail").length
          const isCollapsed = collapsed[g.key]
          return (
            <div key={g.key}>
              <TestGroupHeader
                label={g.label}
                count={tests.length}
                passed={passed}
                failed={failed}
                collapsed={isCollapsed}
                onToggle={() => setCollapsed(prev => ({ ...prev, [g.key]: !prev[g.key] }))}
              />
              {!isCollapsed && g.files.map(f => (
                <TestFileRow key={f.file} file={f} isOpen={expanded === f.file} onToggle={() => setExpanded(expanded === f.file ? null : f.file)} />
              ))}
            </div>
          )
        })}
      </div>
    </div>
  )
}

// ═══════════════════════════════════════════════════════════════
// Tab: Plan
// ═══════════════════════════════════════════════════════════════

function PlanTab() {
  const [content, setContent] = useState(null)
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    fetch("/api/plan")
      .then(r => r.json())
      .then(d => { setContent(d.content || DEMO_PLAN); setLoading(false) })
      .catch(() => { setContent(DEMO_PLAN); setLoading(false) })
  }, [])

  if (loading) return <p className="p-4 color-fg-muted">Loading plan...</p>
  if (!content) return <p className="p-4 color-fg-muted">No plan.md found. Run /plan to create one.</p>

  const lines = content.split("\n")
  const elements = []
  let inCode = false, codeBlock = [], codeLang = ""

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]
    if (line.startsWith("```")) {
      if (inCode) {
        elements.push(<pre key={`c${i}`}><code className={codeLang ? `language-${codeLang}` : ""}>{codeBlock.join("\n")}</code></pre>)
        codeBlock = []; inCode = false; codeLang = ""
      } else { inCode = true; codeLang = line.slice(3).trim() }
      continue
    }
    if (inCode) { codeBlock.push(line); continue }
    if (line.startsWith("# ")) elements.push(<h1 key={i}>{renderInline(line.slice(2))}</h1>)
    else if (line.startsWith("## ")) elements.push(<h2 key={i}>{renderInline(line.slice(3))}</h2>)
    else if (line.startsWith("### ")) elements.push(<h3 key={i}>{renderInline(line.slice(4))}</h3>)
    else if (line.startsWith("- ") || line.startsWith("* ")) elements.push(<li key={i}>{renderInline(line.slice(2))}</li>)
    else if (line.trim() !== "") elements.push(<p key={i}>{renderInline(line)}</p>)
  }

  return <div className="markdown-body markdown-compact p-4">{elements}</div>
}

// ═══════════════════════════════════════════════════════════════
// Tab: Code (GitHub-style inline file viewer)
// ═══════════════════════════════════════════════════════════════

function MarkdownPreview({ content }) {
  const lines = content.split("\n")
  const elements = []
  let inCode = false, codeBlock = [], codeLang = ""
  let listItems = [], listKey = 0

  const flushList = () => {
    if (listItems.length) {
      elements.push(<ul key={`ul${listKey}`}>{listItems}</ul>)
      listItems = []
    }
  }

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]
    if (line.startsWith("```")) {
      if (inCode) {
        flushList()
        const lang = codeLang || undefined
        let html = escapeHtml(codeBlock.join("\n"))
        if (window.hljs && lang) {
          try { html = window.hljs.highlight(codeBlock.join("\n"), { language: lang }).value } catch {}
        }
        elements.push(<pre key={`c${i}`}><code className={lang ? `language-${lang} hljs` : ""} dangerouslySetInnerHTML={{ __html: html }} /></pre>)
        codeBlock = []; inCode = false; codeLang = ""
      } else { inCode = true; codeLang = line.slice(3).trim() }
      continue
    }
    if (inCode) { codeBlock.push(line); continue }
    if (line.startsWith("# ")) { flushList(); elements.push(<h1 key={i}>{renderInline(line.slice(2))}</h1>) }
    else if (line.startsWith("## ")) { flushList(); elements.push(<h2 key={i}>{renderInline(line.slice(3))}</h2>) }
    else if (line.startsWith("### ")) { flushList(); elements.push(<h3 key={i}>{renderInline(line.slice(4))}</h3>) }
    else if (line.startsWith("#### ")) { flushList(); elements.push(<h4 key={i}>{renderInline(line.slice(5))}</h4>) }
    else if (line.startsWith("- ") || line.startsWith("* ")) { listItems.push(<li key={i}>{renderInline(line.slice(2))}</li>); listKey = i }
    else if (/^\d+\.\s/.test(line)) { listItems.push(<li key={i}>{renderInline(line.replace(/^\d+\.\s/, ""))}</li>); listKey = i }
    else if (line.startsWith("> ")) { flushList(); elements.push(<blockquote key={i}><p>{renderInline(line.slice(2))}</p></blockquote>) }
    else if (line.startsWith("---") || line.startsWith("***")) { flushList(); elements.push(<hr key={i} />) }
    else if (line.trim() !== "") { flushList(); elements.push(<p key={i}>{renderInline(line)}</p>) }
    else { flushList() }
  }
  flushList()

  return <div className="markdown-body markdown-compact p-4">{elements}</div>
}

function CodeTab() {
  const [files, setFiles] = useState(null)
  const [filter, setFilter] = useState("")
  const [viewing, setViewing] = useState(null)
  const [currentDir, setCurrentDir] = useState("")
  const [fileContent, setFileContent] = useState(null)
  const [hlLines, setHlLines] = useState([])
  const [fileLoading, setFileLoading] = useState(false)
  const [viewMode, setViewMode] = useState("code")

  useEffect(() => {
    fetch("/api/files")
      .then(r => r.json())
      .then(d => setFiles(d.files?.length ? d.files : DEMO_FILES))
      .catch(() => setFiles(DEMO_FILES))
  }, [])

  useEffect(() => {
    if (!viewing) { setFileContent(null); setHlLines([]); setViewMode("code"); return }
    setFileLoading(true)
    const ext = getFileExt(viewing.path)
    setViewMode(ext === "md" ? "preview" : "code")
    fetch(`/api/file?path=${encodeURIComponent(viewing.path)}`)
      .then(r => { if (!r.ok) throw new Error("not found"); return r.json() })
      .then(d => { setFileContent(d.content ?? ""); setFileLoading(false) })
      .catch(() => {
        const demo = DEMO_CONTENT[viewing.path]
        setFileContent(demo ?? `// ${viewing.path}`)
        setFileLoading(false)
      })
  }, [viewing])

  useEffect(() => {
    if (fileContent == null) return
    const ext = getFileExt(viewing?.path || "")
    const lang = EXT_TO_LANG[ext]
    if (window.hljs && lang) {
      try {
        const result = window.hljs.highlight(fileContent, { language: lang })
        setHlLines(result.value.split("\n"))
      } catch { setHlLines(fileContent.split("\n").map(escapeHtml)) }
    } else {
      setHlLines(fileContent.split("\n").map(escapeHtml))
    }
  }, [fileContent])

  if (!files) return <p className="p-4 color-fg-muted">Loading files...</p>

  // File viewer
  if (viewing) {
    const ext = getFileExt(viewing.path)
    const isMd = ext === "md"
    const badge = FILE_BADGES[ext]
    return (
      <div className="px-4 pt-3 pb-4">
        <div className="d-flex flex-items-center mb-2" style={{ gap: 12 }}>
          <button className="back-btn" type="button" onClick={() => setViewing(null)}>
            <BackIcon /> Back
          </button>
        </div>
        <div className="Box">
          <div className="Box-header d-flex flex-items-center py-2 px-3" style={{ gap: 8 }}>
            <FileIcon size={16} />
            <span className="f6 text-bold text-mono">{viewing.path}</span>
            {badge && <span className="Label" style={{ color: badge.color, borderColor: badge.color + "44", backgroundColor: badge.color + "18" }}>{badge.label}</span>}
            <span className="flex-1" />
            {isMd && (
              <div className="BtnGroup" style={{ marginRight: 8 }}>
                <button type="button" className={`BtnGroup-item btn btn-sm ${viewMode === "code" ? "selected" : ""}`} onClick={() => setViewMode("code")}>Code</button>
                <button type="button" className={`BtnGroup-item btn btn-sm ${viewMode === "preview" ? "selected" : ""}`} onClick={() => setViewMode("preview")}>Preview</button>
              </div>
            )}
            <span className="f6 color-fg-muted">{hlLines.length} lines &middot; {formatSize(viewing.size)}</span>
          </div>
          {fileLoading ? (
            <div className="p-4 color-fg-muted"><span className="anim-spin d-inline-block mr-1">&#x21bb;</span> Loading...</div>
          ) : viewMode === "preview" && isMd && fileContent != null ? (
            <MarkdownPreview content={fileContent} />
          ) : (
            <div className="code-view">
              <table>
                <tbody>
                  {hlLines.map((html, i) => (
                    <tr key={i}>
                      <td className="ln">{i + 1}</td>
                      <td className="lc" dangerouslySetInnerHTML={{ __html: html || " " }} />
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </div>
      </div>
    )
  }

  // File tree with breadcrumbs
  const filtered = filter ? files.filter(f => f.path.toLowerCase().includes(filter.toLowerCase())) : files
  const entries = filter ? getTreeEntries(filtered, "") : getTreeEntries(files, currentDir)

  // Breadcrumb segments
  const breadcrumbs = currentDir ? currentDir.replace(/\/$/, "").split("/") : []

  return (
    <div className="px-4 pt-3 pb-4">
      <input className="form-control width-full mb-2" type="text" placeholder="Filter files..." value={filter} onChange={e => setFilter(e.target.value)} />

      {/* Breadcrumb */}
      <div className="d-flex flex-items-center f5 mb-2" style={{ gap: 4 }}>
        <a className={`${currentDir ? "color-fg-accent" : "text-bold color-fg-default"}`} style={{ cursor: "pointer", textDecoration: "none" }} onClick={() => { setCurrentDir(""); setFilter("") }}>
          <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor" style={{ verticalAlign: "text-bottom", marginRight: 4 }}>
            <path d="M2 2.5A2.5 2.5 0 0 1 4.5 0h8.75a.75.75 0 0 1 .75.75v12.5a.75.75 0 0 1-.75.75h-2.5a.75.75 0 0 1 0-1.5h1.75v-2h-8a1 1 0 0 0-.714 1.7.75.75 0 1 1-1.072 1.05A2.495 2.495 0 0 1 2 11.5Zm10.5-1h-8a1 1 0 0 0-1 1v6.708A2.486 2.486 0 0 1 4.5 9h8ZM5 12.25a.25.25 0 0 1 .25-.25h3.5a.25.25 0 0 1 .25.25v3.25a.25.25 0 0 1-.4.2l-1.45-1.087a.249.249 0 0 0-.3 0L5.4 15.7a.25.25 0 0 1-.4-.2Z" />
          </svg>
          root
        </a>
        {breadcrumbs.map((seg, idx) => {
          const path = breadcrumbs.slice(0, idx + 1).join("/") + "/"
          const isLast = idx === breadcrumbs.length - 1
          return (
            <span key={idx} className="d-flex flex-items-center" style={{ gap: 4 }}>
              <span className="color-fg-muted">/</span>
              <a className={isLast ? "text-bold color-fg-default" : "color-fg-accent"} style={{ cursor: "pointer", textDecoration: "none" }} onClick={() => setCurrentDir(path)}>{seg}</a>
            </span>
          )
        })}
      </div>

      <div className="Box">
        {/* Go up row */}
        {currentDir && !filter && (
          <div className="Box-row file-row d-flex flex-items-center px-3" style={{ gap: 8, paddingTop: 8, paddingBottom: 8, cursor: "pointer" }}
            onClick={() => {
              const parts = currentDir.replace(/\/$/, "").split("/")
              parts.pop()
              setCurrentDir(parts.length ? parts.join("/") + "/" : "")
            }}>
            <span className="color-fg-muted">..</span>
          </div>
        )}
        {entries.map(e => {
          if (e.type === "dir") {
            return (
              <div key={e.path} className="Box-row file-row d-flex flex-items-center px-3" style={{ gap: 8, paddingTop: 8, paddingBottom: 8, cursor: "pointer" }} onClick={() => { setCurrentDir(e.path); setFilter("") }}>
                <FolderIcon />
                <span className="flex-1 f5 text-bold">{e.name}</span>
                <span className="f6 color-fg-subtle">{e.count} items</span>
              </div>
            )
          }
          const ext = getFileExt(e.name)
          const badge = FILE_BADGES[ext]
          return (
            <div key={e.file.path} className="Box-row file-row d-flex flex-items-center px-3" style={{ gap: 8, paddingTop: 8, paddingBottom: 8, cursor: "pointer" }} onClick={() => setViewing(e.file)}>
              <FileIcon />
              <span className="flex-1 f5">{e.name}</span>
              {badge && <span className="Label" style={{ color: badge.color, borderColor: badge.color + "44", backgroundColor: badge.color + "18" }}>{badge.label}</span>}
              <span className="f6 color-fg-subtle text-right" style={{ minWidth: 48 }}>{formatSize(e.file.size)}</span>
            </div>
          )
        })}
      </div>
    </div>
  )
}

// ═══════════════════════════════════════════════════════════════
// Tab: Commands
// ═══════════════════════════════════════════════════════════════

function CommandsTab({ data }) {
  const commands = deriveCommands(data)
  return (
    <div className="markdown-body markdown-compact p-4">
      {Object.entries(commands).map(([section, cmds]) => (
        <div key={section} className="mb-4">
          <h3>{section}</h3>
          {cmds.map(c => (
            <div key={c.cmd} className="mb-2">
              <CodeBlock code={`$ ${c.cmd}`} lang="bash" />
              <p className="f6 color-fg-muted mt-1">{c.desc}</p>
            </div>
          ))}
        </div>
      ))}
    </div>
  )
}

// ═══════════════════════════════════════════════════════════════
// Tab: Skills
// ═══════════════════════════════════════════════════════════════

function SkillsTab() {
  return (
    <div className="markdown-body markdown-compact p-4">
      {Object.entries(SKILLS).map(([section, skills]) => (
        <div key={section} className="mb-4">
          <h3>{section}</h3>
          <table>
            <thead><tr><th>Command</th><th>Type</th><th>Description</th></tr></thead>
            <tbody>
              {skills.map(s => {
                const c = BADGE_COLORS[s.badge] || "#6b7280"
                return (
                  <tr key={s.cmd}>
                    <td><code className="text-bold">{s.cmd}</code></td>
                    <td><span className="skill-badge" style={{ backgroundColor: c }}>{s.badge}</span></td>
                    <td>{s.desc}</td>
                  </tr>
                )
              })}
            </tbody>
          </table>
        </div>
      ))}
      <div className="Box color-bg-subtle p-3 f6 color-fg-muted mt-3">
        Type any skill name in the Claude Code conversation to invoke it. Skills with arguments: <code>/build "feature name"</code>, <code>/test test/aos.test.js</code>
      </div>
    </div>
  )
}

// ═══════════════════════════════════════════════════════════════
// Tab: Deploy
// ═══════════════════════════════════════════════════════════════

const DEPLOY_STEPS = [
  {
    title: "1. Generate Wallet",
    desc: "Create an Arweave JWK wallet for signing deploys. Saved to .wallet.json (gitignored).",
    cmds: [
      { cmd: "yarn keygen", desc: "Generate .wallet.json in project root" },
    ],
  },
  {
    title: "2. Test Lua Scripts",
    desc: "Run your Lua scripts against in-memory AOS to verify correctness before deploying.",
    cmds: [
      { cmd: "yarn test", desc: "Run all unit tests (in-memory AOS)" },
      { cmd: "yarn test test/<name>.test.js", desc: "Run a specific test file" },
    ],
  },
  {
    title: "3. Deploy",
    desc: "yarn deploy runs scripts/deploy.js — it reads all src/*.lua files (or specific files you pass), spawns a process for each, and loads the Lua code. On testnet this uses Eval (sends the source as a message with Action: \"Eval\"). On HyperBEAM it uses ao.deploy() which bundles spawn + source in one call.",
    cmds: [
      { cmd: "yarn deploy", desc: "Deploy all src/*.lua to AO testnet" },
      { cmd: "yarn deploy src/token.lua", desc: "Deploy a single script to testnet" },
      { cmd: "yarn deploy --local-hb", desc: "Deploy to local HyperBEAM (genesis-wasm)" },
      { cmd: "yarn deploy --local-hb --lua", desc: "Deploy to local HyperBEAM (Lua mode — faster, no receive())" },
      { cmd: "yarn deploy --mainnet", desc: "Deploy to remote HyperBEAM (push-1.forward.computer)" },
      { cmd: "yarn deploy --mainnet --lua", desc: "Deploy to remote HyperBEAM (Lua mode)" },
    ],
  },
  {
    title: "4. Verify on Explorers",
    desc: "After deploying, the CLI prints each process ID. Use these explorers to inspect your processes:",
    cmds: [
      { cmd: "https://aolink.ar.io/#/entity/<PROCESS_ID>", desc: "aolink — AOS process explorer (messages, state, tags)" },
      { cmd: "https://lunar.ar.io/#/process/<PROCESS_ID>", desc: "lunar — HyperBEAM explorer (slots, devices, compute logs)" },
    ],
  },
]

// ═══════════════════════════════════════════════════════════════
// Tab: README
// ═══════════════════════════════════════════════════════════════

const DEMO_README = `# Token Transfer App

A decentralized token transfer application built on [AO](https://ao.arweave.dev) and [HyperBEAM](https://docs.wao.eco). Users can mint tokens, transfer them between Arweave wallets, and query balances — all running as permanent, verifiable processes on the AO computer.

## What It Does

- **Mint** — Create new tokens with an initial supply assigned to the minting wallet
- **Transfer** — Send tokens between any two Arweave addresses with atomic balance updates
- **Balance** — Query the balance of any address via dry-run (no gas, no state mutation)
- **Registry** — Register tokens for discoverability so other apps can find and interact with them
- **Frontend** — React SPA with ArConnect wallet integration for browser-based transfers

## How It Works

AOS scripts run as permanent processes on the AO network. Each process has its own state (balances, registry entries) and responds to messages. The frontend connects via ArConnect and sends signed messages to the AOS process.

On HyperBEAM, the same scripts deploy as genesis-wasm devices with slot-based message scheduling. This gives you a local development environment identical to production.

## Project Structure

\`\`\`
src/token.lua                   # Token handler — mint, transfer, balance
src/registry.lua                # Token registry — register, list, query
test/aos.test.js                # In-memory AOS unit tests
test/token.test.js              # Token-specific unit tests
test/registry.test.js           # Registry unit tests
test/hyperbeam-token.test.js    # HyperBEAM integration tests
test/token-device.test.js       # Device HTTP integration tests
frontend/src/App.jsx            # Main app with wallet connect
frontend/src/components/        # TransferForm, BalanceDisplay, etc.
frontend/src/hooks/             # useToken, useWallet
scripts/deploy.js               # Multi-target deploy script
\`\`\`

## Setup

\`\`\`bash
yarn install
yarn keygen          # generate .wallet.json (Arweave JWK)
\`\`\`

## Test

\`\`\`bash
yarn test test/aos.test.js          # in-memory AOS — fast, no server
yarn test test/token.test.js        # token unit tests
yarn test test/registry.test.js     # registry unit tests
yarn test test/hyperbeam.test.js    # HyperBEAM integration (requires local HB)
cd frontend && npm run test:unit    # frontend vitest
cd frontend && npm run test:e2e     # Playwright E2E with live backend
\`\`\`

## Deploy

\`\`\`bash
yarn deploy                    # all src/*.lua to AO testnet
yarn deploy src/token.lua      # single script to testnet
yarn deploy --local-hb         # local HyperBEAM (genesis-wasm)
yarn deploy --local-hb --lua   # local HyperBEAM (Lua mode, faster)
yarn deploy --mainnet          # remote HyperBEAM (push-1.forward.computer)
\`\`\`

Each script gets its own AOS process. On testnet, the source is loaded via Eval message. On HyperBEAM, spawn and source are bundled in a single deploy call.

## AOS Script API

### Token (\`src/token.lua\`)

- \`Action: "Mint"\` — Mint tokens. Tags: \`Quantity\`
- \`Action: "Transfer"\` — Transfer tokens. Tags: \`Recipient\`, \`Quantity\`
- \`Action: "Balance"\` — Query balance. Tags: \`Target\` (optional, defaults to sender)
- \`Action: "Balances"\` — Returns all balances as JSON

### Registry (\`src/registry.lua\`)

- \`Action: "Register"\` — Register a token. Tags: \`ProcessId\`, \`Name\`, \`Ticker\`
- \`Action: "Deregister"\` — Remove a token. Tags: \`ProcessId\`
- \`Action: "List"\` — List all registered tokens
- \`Action: "Info"\` — Query token metadata. Tags: \`ProcessId\`

## Explorers

After deploying, the CLI prints each process ID. Use these to inspect your processes:

- [aolink](https://aolink.ar.io) — AOS process explorer (messages, state, tags)
- [lunar](https://lunar.ar.io) — HyperBEAM explorer (slots, devices, compute logs)

## Built With

- [WAO](https://docs.wao.eco) — SDK for AO and HyperBEAM
- [AOS](https://ao.arweave.dev) — Lua processes on the AO computer
- [HyperBEAM](https://github.com/permaweb/HyperBEAM) — Erlang runtime for AO
- [ArConnect](https://www.arconnect.io) — Arweave wallet browser extension
`

function ReadmeTab() {
  const [content, setContent] = useState(null)
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    fetch("/api/readme")
      .then(r => r.json())
      .then(d => { setContent(d.content || DEMO_README); setLoading(false) })
      .catch(() => { setContent(DEMO_README); setLoading(false) })
  }, [])

  if (loading) return <p className="p-4 color-fg-muted">Loading README...</p>
  if (!content) return <p className="p-4 color-fg-muted">No README.md yet. Run /readme to generate one.</p>

  const lines = content.split("\n")
  const elements = []
  let inCode = false, codeBlock = [], codeLang = ""

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]
    if (line.startsWith("```")) {
      if (inCode) {
        elements.push(<CodeBlock key={i} code={codeBlock.join("\n")} lang={codeLang} />)
        codeBlock = []; inCode = false; codeLang = ""
      } else {
        inCode = true; codeLang = line.slice(3).trim()
      }
    } else if (inCode) {
      codeBlock.push(line)
    } else if (line.startsWith("# ")) {
      elements.push(<h1 key={i}>{line.slice(2)}</h1>)
    } else if (line.startsWith("## ")) {
      elements.push(<h2 key={i}>{line.slice(3)}</h2>)
    } else if (line.startsWith("### ")) {
      elements.push(<h3 key={i}>{line.slice(4)}</h3>)
    } else if (line.startsWith("- ")) {
      const parts = []
      let j = i
      while (j < lines.length && lines[j].startsWith("- ")) {
        parts.push(<li key={j}>{renderInline(lines[j].slice(2))}</li>)
        j++
      }
      elements.push(<ul key={i}>{parts}</ul>)
      i = j - 1
    } else if (line.trim()) {
      elements.push(<p key={i}>{renderInline(line)}</p>)
    }
  }

  return <div className="markdown-body markdown-compact p-4">{elements}</div>
}

// ═══════════════════════════════════════════════════════════════
// Tab: Deploy
// ═══════════════════════════════════════════════════════════════

function DeployTab() {
  const [info, setInfo] = useState(null)

  useEffect(() => {
    fetch("/api/deploy").then(r => r.json()).then(setInfo).catch(() => setInfo(null))
  }, [])

  const hbPort = info?.hyperbeam?.port || "10001"

  return (
    <div className="markdown-body markdown-compact p-4">
      <div className="d-flex flex-items-center flex-justify-between mb-3">
        <h2 style={{ margin: 0, border: "none", paddingBottom: 0 }}>Deploy</h2>
        <span className="f6 color-fg-muted d-flex flex-items-center" style={{ gap: 8 }}>
          <span className="d-flex flex-items-center" style={{ gap: 4 }}>
            <span className="d-inline-block" style={{ width: 8, height: 8, borderRadius: "50%", background: info?.wallet ? "#1a7f37" : "#cf222e" }} />
            {info?.wallet ? "wallet found" : "no wallet"}
          </span>
          &middot;
          <span className="d-flex flex-items-center" style={{ gap: 4 }}>
            <span className="d-inline-block" style={{ width: 8, height: 8, borderRadius: "50%", background: info?.hyperbeam?.configured ? "#1a7f37" : "#bf8700" }} />
            {info?.hyperbeam?.configured ? `HyperBEAM :${hbPort}` : "HyperBEAM not configured"}
          </span>
        </span>
      </div>

      {DEPLOY_STEPS.map(step => (
        <div key={step.title} className="mb-4">
          <h3>{step.title}</h3>
          <p className="f6 color-fg-muted mb-2">{step.desc}</p>
          {step.cmds.map(c => (
            <div key={c.cmd} className="mb-2">
              <CodeBlock code={c.cmd.startsWith("http") ? c.cmd : `$ ${c.cmd}`} lang={c.cmd.startsWith("http") ? "" : "bash"} />
              <p className="f6 color-fg-muted mt-1">{c.desc}</p>
            </div>
          ))}
        </div>
      ))}

      <h3>How Multi-Script Deploy Works</h3>
      <p className="f6 color-fg-muted mb-2">
        When you run <code>yarn deploy</code> without specifying files, <code>scripts/deploy.js</code> reads all <code>src/*.lua</code> files and deploys each one as a separate AOS process:
      </p>
      <ol className="f6 color-fg-muted">
        <li>Loads your wallet from <code>.wallet.json</code></li>
        <li>For each <code>.lua</code> file, spawns a new AOS process</li>
        <li>On <strong>testnet</strong>: sends the Lua source as an <code>Eval</code> message (Action: &quot;Eval&quot;, data = source code)</li>
        <li>On <strong>HyperBEAM</strong>: calls <code>ao.deploy()</code> which bundles spawn + source in one HTTP call</li>
        <li>Prints the process ID for each deployed script</li>
      </ol>

      <h3>Remote HyperBEAM Nodes</h3>
      <p className="f6 color-fg-muted mb-2">Use <code>push-1</code> through <code>push-10</code> for full compute. <code>push.forward.computer</code> is push-only (no compute).</p>

    </div>
  )
}

// ═══════════════════════════════════════════════════════════════
// Main App
// ═══════════════════════════════════════════════════════════════

export default function App() {
  const [data, setData] = useState(DEMO_DATA)
  const [tab, setTab] = useState("tasks")
  const [dark, setDark] = useState(false)
  const [connected, setConnected] = useState(false)

  useEffect(() => {
    const mode = dark ? "dark" : "light"
    document.documentElement.setAttribute("data-color-mode", mode)
    const ls = document.getElementById("hljs-light")
    const ds = document.getElementById("hljs-dark")
    if (ls) ls.disabled = dark
    if (ds) ds.disabled = !dark
  }, [dark])

  useEffect(() => {
    let interval = null
    let es = null

    fetch("/api/progress").then(r => r.json()).then(d => { if (d?.feature) { setData(d); setConnected(true) } }).catch(() => {})

    try {
      es = new EventSource("/api/events")
      es.addEventListener("progress", e => {
        try { const d = JSON.parse(e.data); if (d?.feature) setData(d) } catch {}
      })
      es.onopen = () => setConnected(true)
      es.onerror = () => {
        setConnected(false); es.close(); es = null
        if (!interval) interval = setInterval(() => {
          fetch("/api/progress").then(r => r.json()).then(d => { if (d?.feature) setData(d) }).catch(() => {})
        }, 3000)
      }
    } catch {
      interval = setInterval(() => {
        fetch("/api/progress").then(r => r.json()).then(d => { if (d?.feature) setData(d) }).catch(() => {})
      }, 3000)
    }

    return () => { if (es) es.close(); if (interval) clearInterval(interval) }
  }, [])

  return (
    <div style={{ minHeight: "100vh", display: "flex", flexDirection: "column" }}>
      <Header dark={dark} setDark={setDark} connected={connected} />
      <div style={{ maxWidth: 1012, margin: "0 auto", flex: 1, width: "100%" }}>
        <ProgressSection data={data} />
        <TrackCards data={data} dark={dark} />
        <TabBar active={tab} setActive={setTab} taskCount={data?.tasks?.length || 0} />
        {tab === "tasks" && <TasksTab data={data} />}
        {tab === "tests" && <TestsTab />}
        {tab === "plan" && <PlanTab />}
        {tab === "code" && <CodeTab />}
        {tab === "readme" && <ReadmeTab />}
        {tab === "commands" && <CommandsTab data={data} />}
        {tab === "skills" && <SkillsTab />}
        {tab === "deploy" && <DeployTab />}
      </div>
      <footer style={{ marginTop: 40 }}>
        <div className="d-flex flex-items-center flex-justify-center flex-wrap f6 color-fg-muted" style={{ maxWidth: 1012, margin: "0 auto", padding: "24px 16px", gap: 16 }}>
          <span>&copy; {new Date().getFullYear()} <a href="https://docs.wao.eco/" className="color-fg-muted" style={{ textDecoration: "none" }}>WizardAO</a></span>
          <span>&middot;</span>
          <a href="https://docs.wao.eco/add/overview" className="color-fg-muted" style={{ textDecoration: "none" }}>Docs</a>
          <span>&middot;</span>
          <a href="https://aolink.ar.io" className="color-fg-muted" style={{ textDecoration: "none" }}>aolink</a>
          <span>&middot;</span>
          <a href="https://lunar.ar.io" className="color-fg-muted" style={{ textDecoration: "none" }}>lunar</a>
        </div>
      </footer>
    </div>
  )
}
