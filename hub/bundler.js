const express = require("express")
const cors = require("cors")
const bodyParser = require("body-parser")
const _Arweave = require("arweave")
const Arweave = _Arweave.default ?? _Arweave
const arweave = Arweave.init()
const { generateId, toANS104Request, parseSignatureInput } = require("./utils")

// todo: bundler is unusable due to incorrect ids and signature
const bundler = (sus, cbs, hbs) => {
  const app = express()
  app.use(cors())
  app.use("/tx", bodyParser.raw({ type: "*/*", limit: "100mb" }))
  app.post("/tx", async (req, res) => {
    if (!Buffer.isBuffer(req.body)) {
      console.log("BD: Invalid body | expected raw Buffer")
      return res.status(400).send("Invalid body: expected raw Buffer")
    }
    const lines = req.body.toString("utf8").split(/\r?\n/)
    const sigs = {}
    let currentKey = null
    for (let line of lines) {
      const trimmed = line.trim()
      if (/^--[a-zA-Z0-9_\-=]+/.test(trimmed)) {
        currentKey = null
        continue
      }
      const headerMatch = trimmed.match(/^([a-zA-Z0-9_-]+):\s*(.*)$/)
      if (headerMatch && !headerMatch[2].includes(": ")) {
        const key = headerMatch[1]
        const value = headerMatch[2]
        sigs[key] = value
        currentKey = key
      } else if (currentKey) sigs[currentKey] += "\n" + line
    }
    sigs.target = req.headers.process
    sigs.slot = req.headers.slot

    const input = parseSignatureInput(req.headers["signature-input"])
    const key = { kty: "RSA", n: input.keyid, e: "AQAB" }
    const address = await arweave.wallets.jwkToAddress(key)
    hbs[address] ??= {}
    hbs[address].update = Date.now()
    /*
      const verifier = createVerifier(
      createPublicKey({ key, format: "jwk" }),
      "rsa-pss-sha512"
      )*/
    try {
      /*const isValid = await verifyMessage(
      { keyLookup: params => ({ verify: verifier }) },
      {
        method: req.method,
        headers: req.headers,
        url: `http://ao.com${req.headers.path}`,
      }
      )*/
      const item = toANS104Request(sigs).item
      if ((sigs.slot === "0" || sigs.type === "Process") && sigs.module) {
        for (let v of item.tags) if (v.name === "Type") v.value = "Process"
        await message(
          {
            http_msg: item,
            module: sigs.module,
            scheduler: sigs.scheduler,
            address,
          },
          res
        )
      } else if (sigs.type === "Message") {
        for (let v of item.tags) if (v.name === "id") item.target = v.value
        await message(
          {
            slot: sigs.slot,
            http_msg: item,
            process: sigs.target,
            address,
          },
          res
        )
      } else {
        return res.status(500).send("unknown error")
      }
    } catch (e) {
      return res.status(500).send("unknown error")
    }
  })

  async function message(opt, res) {
    const id = generateId()
    cbs[id] = res
    let exists = false
    for (let k in sus) {
      let hb = sus[k].accept.hb[opt.address]
      let pid = opt.process
      let p = sus[k].accept.pid[pid]
      if (hb?.["*"] || hb?.[pid] || p?.["*"] || p?.[opt.address]) {
        const socket = sus[k].socket
        /*exists = true
        socket.send(
          JSON.stringify({ type: "msg", subtype: "message", id, message: opt })
        )*/
      }
    }
    res.status(200).send("success")
    delete cbs[id]
    /*
    if (!exists) {
      res.status(200).send("success")
      delete cbs[id]
    } else {
      setTimeout(() => {
        if (cbs[id]) {
          res.status(200).send("success")
          delete cbs[id]
        }
      }, 10000)
    }*/
  }

  const server = app.listen(4001, () => console.log(`BD on port 4001`))
}
module.exports = bundler
