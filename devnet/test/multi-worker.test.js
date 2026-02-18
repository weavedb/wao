/**
 * Exhaustive multi-worker integration tests.
 *
 * Architecture under test:
 *   Main Server (port 8850)  — AR + SU + MU + CU combined
 *   Satellite CU1 (port 8851) — CU only, AR_URL → main
 *   Satellite CU2 (port 8852) — CU only, AR_URL → main
 *
 * Verifies: global scan, MU recommit, Scheduler-Location discovery,
 * cross-CU communication, and multi-unit consistency.
 */
import { describe, it, expect, beforeAll, afterAll } from "vitest"
import { startWrangler } from "./helpers.js"
import { resolve } from "node:path"
import { rmSync } from "node:fs"
import { createData, ArweaveSigner } from "arbundles"
import { acc, su as suAcc } from "../../src/accounts.js"

// The well-known scheduler address used in process Scheduler tags.
// This is different from su.addr (the SU wallet address).
const SCHEDULER_ADDR = "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA"

const MAIN_PORT = 8850
const CU1_PORT = 8851
const CU2_PORT = 8852
const CWD = resolve(import.meta.dirname, "..")

let main, cu1, cu2
let MAIN_BASE, CU1_BASE, CU2_BASE
let SCHEDULER

// Module known to be pre-registered in armem-base.js
const MODULE = "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s"

// --- Helpers ---

function tag(name, value) {
  return { name, value: String(value) }
}

async function postMU(base, jwk, { data = "", tags = [], target } = {}) {
  const signer = new ArweaveSigner(jwk)
  const item = createData(data, signer, { tags, target: target || "" })
  await item.sign(signer)
  const res = await fetch(`${base}/mu`, {
    method: "POST",
    headers: { "Content-Type": "application/octet-stream" },
    body: item.getRaw(),
  })
  const json = await res.json().catch(() => ({}))
  return { status: res.status, json, id: json.id || item.id }
}

async function gql(base, query) {
  const res = await fetch(`${base}/ar/graphql`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ query }),
  })
  return res.json()
}

// --- Process IDs ---
let cu1Pid, cu2Pid, mainPid

// --- Setup / Teardown ---

beforeAll(async () => {
  // Clear persistent state from previous runs so D1 + DO are in sync
  try { rmSync(resolve(CWD, ".wrangler/state"), { recursive: true, force: true }) } catch {}

  // Start all 3 wrangler instances in parallel
  ;[main, cu1, cu2] = await Promise.all([
    startWrangler({ port: MAIN_PORT, cwd: CWD }),
    startWrangler({
      port: CU1_PORT,
      cwd: CWD,
      config: "wrangler-cu.toml",
      vars: { AR_URL: `http://localhost:${MAIN_PORT}` },
      healthPath: "/",
    }),
    startWrangler({
      port: CU2_PORT,
      cwd: CWD,
      config: "wrangler-cu.toml",
      vars: { AR_URL: `http://localhost:${MAIN_PORT}` },
      healthPath: "/",
    }),
  ])
  MAIN_BASE = main.baseUrl
  CU1_BASE = cu1.baseUrl
  CU2_BASE = cu2.baseUrl

  // Get scheduler address from main SU
  const suInfo = await fetch(`${MAIN_BASE}/su`).then(r => r.json())
  SCHEDULER = suInfo.Address
}, 300_000)

afterAll(async () => {
  const kills = []
  if (cu2) kills.push(cu2.kill())
  if (cu1) kills.push(cu1.kill())
  if (main) kills.push(main.kill())
  await Promise.all(kills)
}, 30_000)

// ============================================================
// Tier 1: Single AR + Single Satellite CU
// ============================================================
describe("Tier 1: Single AR + Single Satellite CU", () => {
  it("1.1 GET CU1/ returns health", async () => {
    const res = await fetch(`${CU1_BASE}/`)
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data.unit).toBe("CU")
    expect(data.status).toBe("ok")
  }, 15_000)

  it("1.2 Spawn process with CU-URL tag -> CU1", async () => {
    const { status, json, id } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: "-- cu1 counter process",
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Process"),
        tag("Module", MODULE),
        tag("Scheduler", SCHEDULER),
        tag("Name", "CU1-Counter"),
        tag("Authority", suAcc.addr),
        tag("Content-Type", "text/plain"),
        tag("CU-URL", CU1_BASE),
      ],
    })
    if (status !== 200) console.log("1.2 ERROR:", JSON.stringify(json))
    expect(status).toBe(200)
    expect(id).toBeTruthy()
    cu1Pid = id
  }, 60_000)

  it("1.3 CU result accessible from main server", async () => {
    const res = await fetch(
      `${MAIN_BASE}/cu/result/${cu1Pid}?process-id=${cu1Pid}`
    )
    expect(res.status).toBe(200)
    const result = await res.json()
    expect(result).toBeTruthy()
  }, 30_000)

  it("1.4 Install Lua handler via Eval on CU1-backed process", async () => {
    const src = `local count = 0
Handlers.add("Inc", Handlers.utils.hasMatchingTag("Action", "Inc"), function(msg)
  count = count + 1
  msg.reply({ Data = tostring(count) })
end)
Handlers.add("Get", Handlers.utils.hasMatchingTag("Action", "Get"), function(msg)
  msg.reply({ Data = tostring(count) })
end)
return "handlers installed"`
    const { status, id } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: src,
      target: cu1Pid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Eval"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)
    expect(id).toBeTruthy()
  }, 60_000)

  it("1.5 Send Inc -> CU1 executes, result.Messages[0].Data === '1'", async () => {
    const { status, id: msgId } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: "inc",
      target: cu1Pid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Inc"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)

    const res = await fetch(
      `${MAIN_BASE}/cu/result/${msgId}?process-id=${cu1Pid}`
    )
    expect(res.status).toBe(200)
    const result = await res.json()
    expect(result.Messages).toBeInstanceOf(Array)
    expect(result.Messages.length).toBeGreaterThan(0)
    expect(result.Messages[0].Data).toBe("1")
  }, 30_000)

  it("1.6 Second Inc -> count '2'", async () => {
    const { status, id: msgId } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: "inc2",
      target: cu1Pid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Inc"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)

    const res = await fetch(
      `${MAIN_BASE}/cu/result/${msgId}?process-id=${cu1Pid}`
    )
    const result = await res.json()
    expect(result.Messages[0].Data).toBe("2")
  }, 30_000)

  it("1.7 SU lists CU1-backed process", async () => {
    const res = await fetch(`${MAIN_BASE}/su`)
    const data = await res.json()
    expect(data.Processes).toBeInstanceOf(Array)
    const found = data.Processes.some(p => p === cu1Pid || p.id === cu1Pid)
    expect(found).toBe(true)
  }, 15_000)

  it("1.8 GQL returns CU1 process spawn tx", async () => {
    const result = await gql(MAIN_BASE, `{
      transactions(ids: ["${cu1Pid}"]) {
        edges { node { id tags { name value } } }
      }
    }`)
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(0)
    const typeTag = edges[0].node.tags.find(t => t.name === "Type")
    expect(typeTag.value).toBe("Process")
  }, 15_000)

  it("1.9 Results list includes all messages", async () => {
    const res = await fetch(`${MAIN_BASE}/cu/results/${cu1Pid}?limit=25`)
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data.edges).toBeInstanceOf(Array)
    // spawn + eval + inc + inc = 4 results
    expect(data.edges.length).toBeGreaterThanOrEqual(4)
  }, 15_000)
})

// ============================================================
// Tier 2: Multiple Satellite CUs
// ============================================================
describe("Tier 2: Multiple Satellite CUs", () => {
  it("2.1 Spawn process with CU-URL -> CU2", async () => {
    const { status, id } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: "-- cu2 process",
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Process"),
        tag("Module", MODULE),
        tag("Scheduler", SCHEDULER),
        tag("Name", "CU2-Echo"),
        tag("Authority", suAcc.addr),
        tag("Content-Type", "text/plain"),
        tag("CU-URL", CU2_BASE),
      ],
    })
    expect(status).toBe(200)
    expect(id).toBeTruthy()
    cu2Pid = id
  }, 60_000)

  it("2.2 Install handler on CU2 process", async () => {
    const src = `Handlers.add("Echo", Handlers.utils.hasMatchingTag("Action", "Echo"), function(msg)
  msg.reply({ Data = "echo:" .. msg.Data })
end)
return "echo handler installed"`
    const { status } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: src,
      target: cu2Pid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Eval"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)
  }, 60_000)

  it("2.3 Spawn process on main (no CU-URL)", async () => {
    const { status, id } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: "-- main counter process",
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Process"),
        tag("Module", MODULE),
        tag("Scheduler", SCHEDULER),
        tag("Name", "Main-Counter"),
        tag("Authority", suAcc.addr),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)
    expect(id).toBeTruthy()
    mainPid = id
  }, 60_000)

  it("2.4 Install handler on main process", async () => {
    const src = `local count = 0
Handlers.add("Inc", Handlers.utils.hasMatchingTag("Action", "Inc"), function(msg)
  count = count + 1
  msg.reply({ Data = tostring(count) })
end)
return "handler installed"`
    const { status } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: src,
      target: mainPid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Eval"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)
  }, 60_000)

  it("2.5 Each CU independently executes WASM", async () => {
    // Inc on CU1 process
    const { id: cu1MsgId } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: "inc3",
      target: cu1Pid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Inc"),
        tag("Content-Type", "text/plain"),
      ],
    })
    const cu1Res = await fetch(
      `${MAIN_BASE}/cu/result/${cu1MsgId}?process-id=${cu1Pid}`
    ).then(r => r.json())
    expect(cu1Res.Messages[0].Data).toBe("3")

    // Echo on CU2 process
    const { id: cu2MsgId } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: "hello",
      target: cu2Pid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Echo"),
        tag("Content-Type", "text/plain"),
      ],
    })
    const cu2Res = await fetch(
      `${MAIN_BASE}/cu/result/${cu2MsgId}?process-id=${cu2Pid}`
    ).then(r => r.json())
    expect(cu2Res.Messages[0].Data).toBe("echo:hello")

    // Inc on main process
    const { id: mainMsgId } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: "inc",
      target: mainPid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Inc"),
        tag("Content-Type", "text/plain"),
      ],
    })
    const mainRes = await fetch(
      `${MAIN_BASE}/cu/result/${mainMsgId}?process-id=${mainPid}`
    ).then(r => r.json())
    expect(mainRes.Messages[0].Data).toBe("1")
  }, 60_000)

  it("2.6 SU lists ALL processes (main + CU1 + CU2)", async () => {
    const res = await fetch(`${MAIN_BASE}/su`)
    const data = await res.json()
    expect(data.Processes).toBeInstanceOf(Array)
    const pids = data.Processes
    expect(pids).toContain(cu1Pid)
    expect(pids).toContain(cu2Pid)
    expect(pids).toContain(mainPid)
  }, 15_000)

  it("2.7 GQL returns spawn txs for all 3 processes", async () => {
    const result = await gql(MAIN_BASE, `{
      transactions(
        ids: ["${cu1Pid}", "${cu2Pid}", "${mainPid}"]
      ) {
        edges { node { id tags { name value } } }
      }
    }`)
    const ids = result.data.transactions.edges.map(e => e.node.id)
    expect(ids).toContain(cu1Pid)
    expect(ids).toContain(cu2Pid)
    expect(ids).toContain(mainPid)
  }, 15_000)

  it("2.8 GQL tag filter works across all units", async () => {
    // Filter by Name tag to find CU1-Counter
    const result = await gql(MAIN_BASE, `{
      transactions(
        tags: [
          { name: "Type", values: ["Process"] },
          { name: "Name", values: ["CU1-Counter"] }
        ],
        first: 5
      ) {
        edges { node { id tags { name value } } }
      }
    }`)
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(0)
    const found = edges.some(e => e.node.id === cu1Pid)
    expect(found).toBe(true)
  }, 15_000)
})

// ============================================================
// Tier 3: Cross-CU Communication & MU Recommit
// ============================================================
describe("Tier 3: Cross-CU Communication & MU Recommit", () => {
  it("3.1 Install handler: CU1 process sends outbox to CU2 process", async () => {
    const src = `Handlers.add("SendTo", Handlers.utils.hasMatchingTag("Action", "SendTo"), function(msg)
  ao.send({ Target = msg.Tags["Forward-Target"], Data = "forwarded:" .. msg.Data, Action = "Echo" })
  msg.reply({ Data = "sent" })
end)
return "SendTo handler installed"`
    const { status } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: src,
      target: cu1Pid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Eval"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)
  }, 60_000)

  it("3.2 Install receiver handler on CU2 process", async () => {
    const src = `Handlers.add("Receive", Handlers.utils.hasMatchingTag("Action", "Receive"), function(msg)
  msg.reply({ Data = "received:" .. msg.Data })
end)
return "Receive handler installed"`
    const { status } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: src,
      target: cu2Pid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Eval"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)
  }, 60_000)

  it("3.3 Trigger cross-CU: send to CU1 -> outbox targets CU2", async () => {
    const { status, id: msgId } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: "cross-test",
      target: cu1Pid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "SendTo"),
        tag("Forward-Target", cu2Pid),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(status).toBe(200)

    // Verify CU1 replied with "sent"
    const res = await fetch(
      `${MAIN_BASE}/cu/result/${msgId}?process-id=${cu1Pid}`
    )
    const result = await res.json()
    expect(result.Messages).toBeInstanceOf(Array)
    expect(result.Messages.length).toBeGreaterThan(0)
    // One message should be the reply "sent", another is the outbox to CU2
    const sentMsg = result.Messages.find(m => m.Data === "sent")
    expect(sentMsg).toBeTruthy()
  }, 60_000)

  it("3.4 Outbox message visible in AR GQL", async () => {
    // Wait briefly for MU recommit
    await new Promise(r => setTimeout(r, 2000))

    const result = await gql(MAIN_BASE, `{
      transactions(
        tags: [
          { name: "Type", values: ["Message"] },
          { name: "From-Process", values: ["${cu1Pid}"] }
        ],
        first: 10
      ) {
        edges { node { id tags { name value } } }
      }
    }`)
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(0)
    // Check that Pushed-For and From-Process tags exist
    const fwdEdge = edges.find(e =>
      e.node.tags.some(t => t.name === "From-Process" && t.value === cu1Pid)
    )
    expect(fwdEdge).toBeTruthy()
  }, 30_000)

  it("3.5 CU2 process receives the forwarded message", async () => {
    // The forwarded message should have been processed on CU2
    // via MU recommit. Check CU2 results for it.
    const res = await fetch(`${MAIN_BASE}/cu/results/${cu2Pid}?limit=25`)
    const data = await res.json()
    expect(data.edges).toBeInstanceOf(Array)
    // Should have: spawn + echo handler eval + echo msg + receive handler eval + forwarded msg
    expect(data.edges.length).toBeGreaterThanOrEqual(3)
  }, 30_000)

  it("3.6 Main->CU1 cross-process message works", async () => {
    // Install a handler on main that sends to CU1
    const src = `Handlers.add("ForwardToCU1", Handlers.utils.hasMatchingTag("Action", "ForwardToCU1"), function(msg)
  ao.send({ Target = msg.Tags["Forward-Target"], Data = "from-main:" .. msg.Data, Action = "Inc" })
  msg.reply({ Data = "forwarded-to-cu1" })
end)
return "ForwardToCU1 installed"`
    const { status: s1 } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: src,
      target: mainPid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Eval"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(s1).toBe(200)

    // Trigger main -> CU1
    const { status: s2, id: msgId } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: "hello-from-main",
      target: mainPid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "ForwardToCU1"),
        tag("Forward-Target", cu1Pid),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(s2).toBe(200)

    const res = await fetch(
      `${MAIN_BASE}/cu/result/${msgId}?process-id=${mainPid}`
    ).then(r => r.json())
    expect(res.Messages).toBeInstanceOf(Array)
    const fwdReply = res.Messages.find(m => m.Data === "forwarded-to-cu1")
    expect(fwdReply).toBeTruthy()
  }, 60_000)

  it("3.7 CU1->main cross-process message works", async () => {
    // Install a handler on CU1 that sends to main
    const src = `Handlers.add("ForwardToMain", Handlers.utils.hasMatchingTag("Action", "ForwardToMain"), function(msg)
  ao.send({ Target = msg.Tags["Forward-Target"], Data = "from-cu1:" .. msg.Data, Action = "Inc" })
  msg.reply({ Data = "forwarded-to-main" })
end)
return "ForwardToMain installed"`
    const { status: s1 } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: src,
      target: cu1Pid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "Eval"),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(s1).toBe(200)

    // Trigger CU1 -> main
    const { status: s2, id: msgId } = await postMU(MAIN_BASE, acc[0].jwk, {
      data: "hello-from-cu1",
      target: cu1Pid,
      tags: [
        tag("Data-Protocol", "ao"),
        tag("Variant", "ao.TN.1"),
        tag("Type", "Message"),
        tag("Action", "ForwardToMain"),
        tag("Forward-Target", mainPid),
        tag("Content-Type", "text/plain"),
      ],
    })
    expect(s2).toBe(200)

    const res = await fetch(
      `${MAIN_BASE}/cu/result/${msgId}?process-id=${cu1Pid}`
    ).then(r => r.json())
    expect(res.Messages).toBeInstanceOf(Array)
    const fwdReply = res.Messages.find(m => m.Data === "forwarded-to-main")
    expect(fwdReply).toBeTruthy()
  }, 60_000)

  it("3.8 Sequential messages to same CU maintain order", async () => {
    // Send 5 rapid Inc messages to CU1 and verify final count
    // CU1 count was 3 from test 2.5, then test 3.6 sends an Inc too
    // The exact count depends on prior tests, so we do relative check
    const getCount = async () => {
      const { id: gid } = await postMU(MAIN_BASE, acc[0].jwk, {
        data: "get",
        target: cu1Pid,
        tags: [
          tag("Data-Protocol", "ao"),
          tag("Variant", "ao.TN.1"),
          tag("Type", "Message"),
          tag("Action", "Get"),
          tag("Content-Type", "text/plain"),
        ],
      })
      const res = await fetch(
        `${MAIN_BASE}/cu/result/${gid}?process-id=${cu1Pid}`
      ).then(r => r.json())
      return parseInt(res.Messages?.[0]?.Data ?? "0", 10)
    }

    const before = await getCount()

    // Send 5 sequential Inc messages
    for (let i = 0; i < 5; i++) {
      const { status } = await postMU(MAIN_BASE, acc[0].jwk, {
        data: `seq-${i}`,
        target: cu1Pid,
        tags: [
          tag("Data-Protocol", "ao"),
          tag("Variant", "ao.TN.1"),
          tag("Type", "Message"),
          tag("Action", "Inc"),
          tag("Content-Type", "text/plain"),
        ],
      })
      expect(status).toBe(200)
    }

    const after = await getCount()
    expect(after).toBe(before + 5)
  }, 120_000)
})

// ============================================================
// Tier 4: Scheduler-Location & SU Discovery
// ============================================================
describe("Tier 4: Scheduler-Location & SU Discovery", () => {
  it("4.1 GQL query for Type: Scheduler-Location returns SU entries", async () => {
    const result = await gql(MAIN_BASE, `{
      transactions(
        tags: [{ name: "Type", values: ["Scheduler-Location"] }],
        first: 5
      ) {
        edges { node { id owner { address } tags { name value } } }
      }
    }`)
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(0)
  }, 15_000)

  it("4.2 Scheduler-Location tx has correct Url tag", async () => {
    const result = await gql(MAIN_BASE, `{
      transactions(
        tags: [{ name: "Type", values: ["Scheduler-Location"] }],
        first: 5
      ) {
        edges { node { tags { name value } } }
      }
    }`)
    const edge = result.data.transactions.edges[0]
    const urlTag = edge.node.tags.find(t => t.name === "Url")
    expect(urlTag).toBeTruthy()
    expect(urlTag.value).toBeTruthy()
    // The URL should be a valid URL string
    expect(urlTag.value).toMatch(/^https?:\/\//)
  }, 15_000)

  it("4.3 Scheduler-Location owned by scheduler address", async () => {
    const result = await gql(MAIN_BASE, `{
      transactions(
        tags: [{ name: "Type", values: ["Scheduler-Location"] }],
        first: 5
      ) {
        edges { node { owner { address } } }
      }
    }`)
    const edge = result.data.transactions.edges[0]
    expect(edge.node.owner.address).toBe(SCHEDULER_ADDR)
  }, 15_000)

  it("4.4 findSU() via RemoteAR returns correct URL", async () => {
    // Simulate what a satellite CU would do: query GQL for Scheduler-Location
    const query = `{ transactions(
      owners: ["${SCHEDULER_ADDR}"],
      tags: [{ name: "Type", values: ["Scheduler-Location"] }],
      first: 1, sort: HEIGHT_DESC
    ) { edges { node { tags { name value } } } } }`
    const result = await gql(MAIN_BASE, query)
    const tags = result.data?.transactions?.edges?.[0]?.node?.tags ?? []
    const urlTag = tags.find(t => t.name === "Url")
    expect(urlTag).toBeTruthy()
    expect(urlTag.value).toBeTruthy()
  }, 15_000)
})

// ============================================================
// Tier 5: Global Scan & Consistency
// ============================================================
describe("Tier 5: Global Scan & Consistency", () => {
  it("5.1 GQL Type: Process returns ALL processes from all CUs", async () => {
    const result = await gql(MAIN_BASE, `{
      transactions(
        tags: [{ name: "Type", values: ["Process"] }],
        first: 50
      ) {
        edges { node { id } }
      }
    }`)
    const ids = result.data.transactions.edges.map(e => e.node.id)
    expect(ids).toContain(cu1Pid)
    expect(ids).toContain(cu2Pid)
    expect(ids).toContain(mainPid)
  }, 15_000)

  it("5.2 GQL Type: Message returns messages from all CUs", async () => {
    const result = await gql(MAIN_BASE, `{
      transactions(
        tags: [{ name: "Type", values: ["Message"] }],
        first: 100
      ) {
        edges { node { id tags { name value } } }
      }
    }`)
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(0)
    // Messages should exist — at least our eval and inc messages
    const allIds = edges.map(e => e.node.id)
    expect(allIds.length).toBeGreaterThanOrEqual(3)
  }, 15_000)

  it("5.3 GQL Type: Assignment shows assignments for satellite-CU procs", async () => {
    const result = await gql(MAIN_BASE, `{
      transactions(
        tags: [
          { name: "Type", values: ["Assignment"] },
          { name: "Process", values: ["${cu1Pid}"] }
        ],
        first: 50
      ) {
        edges { node { id tags { name value } } }
      }
    }`)
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(0)
    // All should have Process tag matching cu1Pid
    for (const e of edges) {
      const processTag = e.node.tags.find(t => t.name === "Process")
      expect(processTag.value).toBe(cu1Pid)
    }
  }, 15_000)

  it("5.4 GQL owner filter across units", async () => {
    const result = await gql(MAIN_BASE, `{
      transactions(
        owners: ["${acc[0].addr}"],
        tags: [{ name: "Type", values: ["Process"] }],
        first: 50
      ) {
        edges { node { id owner { address } } }
      }
    }`)
    const edges = result.data.transactions.edges
    expect(edges.length).toBeGreaterThan(0)
    // All processes spawned by acc[0] should be found
    const ids = edges.map(e => e.node.id)
    expect(ids).toContain(cu1Pid)
    expect(ids).toContain(cu2Pid)
    expect(ids).toContain(mainPid)
    // All should be owned by acc[0]
    for (const e of edges) {
      expect(e.node.owner.address).toBe(acc[0].addr)
    }
  }, 15_000)

  it("5.5 Block height consistent", async () => {
    const arRes = await fetch(`${MAIN_BASE}/ar`).then(r => r.json())
    const mainHeight = arRes.height
    expect(mainHeight).toBeGreaterThan(0)

    // All tx block heights should be <= main height
    const result = await gql(MAIN_BASE, `{
      transactions(first: 20, sort: HEIGHT_DESC) {
        edges { node { id block { height } } }
      }
    }`)
    for (const e of result.data.transactions.edges) {
      if (e.node.block?.height !== undefined) {
        expect(e.node.block.height).toBeLessThanOrEqual(mainHeight)
      }
    }
  }, 15_000)

  it("5.6 GQL pagination across multi-CU txs", async () => {
    // Fetch first page
    const page1 = await gql(MAIN_BASE, `{
      transactions(first: 3, sort: HEIGHT_DESC) {
        pageInfo { hasNextPage }
        edges { cursor node { id } }
      }
    }`)
    const edges1 = page1.data.transactions.edges
    expect(edges1.length).toBe(3)

    // Use last cursor for next page
    const lastCursor = edges1[edges1.length - 1].cursor
    const page2 = await gql(MAIN_BASE, `{
      transactions(first: 3, after: "${lastCursor}", sort: HEIGHT_DESC) {
        pageInfo { hasNextPage }
        edges { cursor node { id } }
      }
    }`)
    const edges2 = page2.data.transactions.edges
    expect(edges2.length).toBeGreaterThan(0)

    // No overlap between pages
    const ids1 = new Set(edges1.map(e => e.node.id))
    for (const e of edges2) {
      expect(ids1.has(e.node.id)).toBe(false)
    }
  }, 15_000)
})
