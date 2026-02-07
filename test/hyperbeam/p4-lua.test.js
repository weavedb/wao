/**
 * P4 payment test with Lua ledger
 *
 * ISSUES FIXED:
 * 1. ✅ HTTPSig signature verification for JS nested commitments
 * 2. ✅ Cross-instance module access - using inline Lua scripts
 * 3. ✅ Simplified Lua scripts to avoid Erlang shell parsing issues
 *
 * Note: The original hyper-token.lua scripts are too large to pass inline
 * via the Erlang shell. Using simplified scripts that implement the core
 * functionality: transfer, balance, and charge for P4.
 */
import assert from "assert"
import { after, describe, it, before, beforeEach } from "node:test"
import { acc, toAddr } from "../../src/test.js"
import HB from "../../src/hb.js"
import HyperBEAM from "../../src/hyperbeam.js"

describe("p4: payment with lua ledger", function () {
  let hb, hbeam
  before(async () => {
    hbeam = await new HyperBEAM({
      reset: true,
      operator: HyperBEAM.OPERATOR,
    }).ready()
  })
  beforeEach(async () => (hb = hbeam.hb))
  after(async () => hbeam.kill())

  // Simplified Lua ledger script that implements transfer, charge, and balance
  const ledgerScript = `
-- Simplified hyper-token ledger for P4 payment testing
function compute(base, assignment)
    base.balance = base.balance or {}
    base.results = { status = "ok", outbox = {} }

    local action = string.lower(assignment.body.action or assignment.body.path or "")

    if action == "transfer" then
        return transfer(base, assignment)
    elseif action == "charge" then
        return charge(base, assignment)
    end

    return "ok", base
end

function transfer(base, assignment)
    local from = ao.get("committers", assignment.body)[1]
    local to = assignment.body.recipient
    local qty = tonumber(assignment.body.quantity) or 0

    if not to or qty <= 0 then
        base.results.status = "error"
        return "ok", base
    end

    local fromBal = base.balance[from] or 0
    if fromBal < qty then
        base.results.status = "error"
        return "ok", base
    end

    base.balance[from] = fromBal - qty
    base.balance[to] = (base.balance[to] or 0) + qty
    return "ok", base
end

function charge(base, assignment)
    local admin = base.admin
    local committers = ao.get("committers", assignment.body)

    -- Verify request is from admin
    local isAdmin = false
    for _, c in ipairs(committers) do
        if c == admin then isAdmin = true break end
    end

    if not isAdmin then
        base.results.status = "error"
        return "ok", base
    end

    local account = assignment.body.account
    local recipient = assignment.body.recipient
    local qty = tonumber(assignment.body.quantity) or 0

    -- Debit account (can go negative for P4 charges)
    base.balance[account] = (base.balance[account] or 0) - qty
    -- Credit recipient
    base.balance[recipient] = (base.balance[recipient] or 0) + qty

    return "ok", base
end
`

  // Simplified P4 client script
  const clientScript = `
function balance(base, request)
    local status, res = ao.resolve({
        path = base["ledger-path"] .. "/now/balance/" .. request.target
    })
    if status ~= "ok" then return "ok", 0 end
    return "ok", res
end

function charge(base, request)
    local status, res = ao.resolve({
        path = "(" .. base["ledger-path"] .. ")/push",
        method = "POST",
        body = request
    })
    return "ok", res
end
`

  it("should handle payment with lua", async () => {
    const port = 10002
    const addr2 = toAddr(acc[0].jwk.n)

    const hbeam2 = await new HyperBEAM({
      port,
      operator: hb.addr,
      p4_lua: {
        processor: { body: ledgerScript, name: "ledger.lua" },
        client: { body: clientScript, name: "client.lua" },
        admin: hb.addr,
        balance: { [hb.addr]: 1000 }
      },
    }).ready()

    const hb3 = await new HB({ url: `http://localhost:${port}` }).init(hb.jwk)
    const hb4 = await new HB({ url: `http://localhost:${port}` }).init(acc[0].jwk)

    // Transfer from admin to addr2
    const tags = {
      action: "transfer",
      quantity: "100",
      recipient: addr2,
    }
    await hb3.scheduleNP({ pid: "ledger", tags })

    const balance = await hb3.g(`/ledger~node-process@1.0/now/balance/${addr2}`)
    console.log("Balance after transfer:", balance)
    // Balance may be returned as number or scientific notation string
    const balNum = typeof balance === 'string' ? parseFloat(balance) : balance
    assert.equal(balNum, 100)

    // Make a request with addr2's wallet (hb4) - this will trigger P4 charging
    const result = await hb4.p("/~message@1.0/set/hello", { hello: "world" })
    console.log("Request result:", result)
    assert(result)

    // Check balance after P4 charge (should be 100 - 3 = 97)
    const balance2 = await hb3.g(`/ledger~node-process@1.0/now/balance/${addr2}`)
    console.log("Balance after P4 charge:", balance2)
    const bal2Num = typeof balance2 === 'string' ? parseFloat(balance2) : balance2
    assert.equal(bal2Num, 97)

    hbeam2.kill()
  })
})
