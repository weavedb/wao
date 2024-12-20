import express from "express"
import cors from "cors"
import base64url from "base64url"
import { DataItem } from "arbundles"
import { tags } from "./utils.js"
import { connect } from "./aoconnect.js"
import { GQL, cu, su, mu } from "./test.js"
import bodyParser from "body-parser"
import { graphql, parse, validate, buildSchema } from "graphql"
import { keys, map, reverse } from "ramda"

const schema = buildSchema(`
type Query {
  transaction(id: ID!): Transaction
  transactions(
    ids: [ID!]
    owners: [String!]
    recipients: [String!]
    tags: [TagFilter!]
    bundledIn: [ID!]
    block: BlockFilter
    first: Int = 10
    after: String
    sort: SortOrder = HEIGHT_DESC
  ): TransactionConnection!
  block(id: String): Block
  blocks(
    ids: [ID!]
    height: BlockFilter
    first: Int = 10
    after: String
    sort: SortOrder = HEIGHT_DESC
  ): BlockConnection!
}

enum SortOrder {
  HEIGHT_ASC
  HEIGHT_DESC
}

input TagFilter {
  name: String!
  values: [String!]!
  op: TagOperator = EQ
}

input BlockFilter {
  min: Int
  max: Int
}

type BlockConnection {
  pageInfo: PageInfo!
  edges: [BlockEdge!]!
}

type BlockEdge {
  cursor: String!
  node: Block!
}

type TransactionConnection {
  pageInfo: PageInfo!
  edges: [TransactionEdge!]!
}

type TransactionEdge {
  cursor: String!
  node: Transaction!
}

type PageInfo {
  hasNextPage: Boolean!
}

type Transaction {
  id: ID!
  anchor: String!
  signature: String!
  recipient: String!
  owner: Owner!
  fee: Amount!
  quantity: Amount!
  data: MetaData!
  tags: [Tag!]!
  block: Block
  parent: Parent @deprecated(reason: "Use \`bundledIn\`")
  bundledIn: Bundle
}

type Parent {
  id: ID!
}

type Bundle {
  id: ID!
}

type Block {
  id: ID!
  timestamp: Int!
  height: Int!
  previous: ID!
}

type MetaData {
  size: String!
  type: String
}

type Amount {
  winston: String!
  ar: String!
}

type Owner {
  address: String!
  key: String!
}

type Tag {
  name: String!
  value: String!
}

enum TagOperator {
  EQ
  NEQ
}
`)

const root = {
  transactions: ({ first }) => ({
    edges: [],
    pageInfo: {
      hasNextPage: false,
    },
  }),
  block: ({ id }) => ({
    id,
    timestamp: Date.now(),
    height: 123456,
    previous: "previous-block-id",
    transactions: [],
    miner: "example-miner",
    reward: "1000",
    tags: [],
    indepHash: "example-indep-hash",
    nonce: "000000",
  }),
}

const mapParsed = (parsedQuery, variables) => {
  const operation = parsedQuery.definitions[0]

  if (operation.operation !== "query") {
    throw new Error("Only 'query' operations are supported.")
  }

  const rootField = operation.selectionSet.selections[0]
  const rootFieldName = rootField.name.value

  const parseArgumentValue = argValue => {
    if (argValue.kind === "Variable") {
      return variables[argValue.name.value]
    }

    if (argValue.kind === "ListValue") {
      return argValue.values.map(value => parseArgumentValue(value))
    }

    if (argValue.kind === "ObjectValue") {
      return argValue.fields.reduce((obj, field) => {
        obj[field.name.value] = parseArgumentValue(field.value)
        return obj
      }, {})
    }

    return argValue.value
  }

  const args = rootField.arguments.reduce((acc, arg) => {
    acc[arg.name.value] = parseArgumentValue(arg.value)
    return acc
  }, {})

  const extractFields = selectionSet => {
    return selectionSet.selections.map(selection => {
      const fieldName = selection.name.value

      if (selection.selectionSet) {
        return {
          [fieldName]: extractFields(selection.selectionSet),
        }
      }
      return fieldName
    })
  }

  const fields = extractFields(rootField.selectionSet)

  return { rootFieldName, args, fields }
}

class Server {
  constructor({ ar = 4000, mu = 4002, su = 4003, cu = 4004, aoconnect } = {}) {
    const {
      ar: _ar,
      message,
      spawn,
      dryrun,
      result,
      results,
      mem,
      monitor,
      unmonitor,
    } = connect(aoconnect)
    this.monitor = monitor
    this.unmonitor = unmonitor
    this.spawn = spawn
    this._ar = _ar
    this.message = message
    this.dryrun = dryrun
    this.result = result
    this.results = results
    this.mem = mem
    this.gql = new GQL({ mem })
    this.ports = { ar, mu, su, cu }
    this.servers = []
    this.ar()
    this.mu()
    this.su()
    this.cu()
  }
  ar() {
    const app = express()
    app.use(cors())
    app.use(bodyParser.json({ limit: "100mb" }))
    app.get("/wallet/:id/balance", (req, res) => res.send("0"))
    app.get("/mint/:id/:amount", (req, res) => res.json({ id: "0" }))
    app.get("/tx/:id/offset", async (req, res) => {
      res.status(400)
      res.send(null)
    })
    app.get("/tx_anchor", async (req, res) => {
      res.send(this.mem.getAnchor())
    })
    app.get("/mine", async (req, res) => {
      res.json(req.params)
    })
    app.get("/:id", async (req, res) => {
      const _data = await this._ar.data(req.params.id)
      if (!_data) {
        res.status(404)
        res.send(null)
      } else {
        res.send(Buffer.from(_data, "base64"))
      }
    })
    app.get("/price/:id", async (req, res) => {
      res.send("0")
    })
    app.post("/graphql", async (req, res) => {
      try {
        const { query, variables } = req.body
        const parsedQuery = parse(query)
        const errors = validate(schema, parsedQuery)
        const parsed = mapParsed(parsedQuery, variables)
        const tar = parsed.rootFieldName
        let fields = null
        for (const v of parsed.fields) {
          if (v.edges) {
            for (const v2 of v.edges) {
              if (v2.node) {
                fields = v2.node
                break
              }
            }
          }
        }
        const args = parsed.args
        if (args.block) {
          for (const k in args.block) args.block[k] *= 1
        }
        if (args.first) args.first *= 1
        if (fields) args.fields = fields
        if (args.sort && args.sort === "HEIGHT_ASC") args.asc = true
        delete args.sort
        if (args.tags) {
          let _tags = {}
          for (const v of args.tags) _tags[v.name] = v.values
          args.tags = _tags
        }
        let res2 = null
        if (tar === "transactions") {
          res2 = await this.gql.txs({ ...args })
        } else if (tar === "blocks") {
          res2 = await this.gql.blocks({ ...args })
        }
        const edges = map(v => ({ node: v, cursor: v.cursor }), res2)
        res.json({
          data: { transactions: { pageInfo: { hasNextPage: true }, edges } },
        })
      } catch (e) {
        console.log(e)
        res.status(400)
        res.json({ error: "bad request" })
      }
    })
    let data = {}
    app.post("/:id", async (req, res) => {
      // id = "tx" | "chunk"
      if (req.body.chunk) {
        if (data[req.body.data_root]) {
          data[req.body.data_root].data += req.body.chunk
          const buf = Buffer.from(req.body.chunk, "base64")
          if (!data[req.body.data_root].chunks) {
            data[req.body.data_root].chunks = buf
          } else {
            data[req.body.data_root].chunks = Buffer.concat([
              data[req.body.data_root].chunks,
              buf,
            ])
          }
          delete req.body.chunk
          if (
            data[req.body.data_root].data_size <=
            data[req.body.data_root].chunks.length
          ) {
            data[req.body.data_root].data =
              data[req.body.data_root].chunks.toString("base64")
            await this._ar.postTx(data[req.body.data_root])
            delete data[req.body.data_root]
          }
        }
        res.json({ id: req.body.id })
      } else {
        if (req.body.data_root && req.body.data === "") {
          data[req.body.data_root] = req.body
        } else {
          await this._ar.postTx(req.body)
        }
        res.json({ id: req.body.id })
      }
    })

    const server = app.listen(this.ports.ar, () => {
      console.log(`AR on port ${this.ports.ar}`)
    })
    this.servers.push(server)
  }
  mu() {
    const app = express()
    app.use(cors())
    app.use(express.raw({ type: "*/*" }))
    app.get("/", (req, res) => res.send("ao messenger unit"))
    app.post("/monitor/:id", async (req, res) => {
      await this.monitor({ process: req.params.id })
      res.json({ id: req.params.id, messag: "cron monitored!" })
    })
    app.delete("/monitor/:id", async (req, res) => {
      await this.unmonitor({ process: req.params.id })
      res.json({ id: req.params.id, message: "cron deleted!" })
    })
    app.post("/", async (req, res) => {
      const binary = req.body
      let valid = await DataItem.verify(binary)
      let type = null
      let item = null
      if (valid) item = new DataItem(binary)
      const _tags = tags(item.tags)
      let err = null
      if (_tags.Type === "Process") {
        await this.spawn({
          item,
          module: _tags.Module,
          scheduler: _tags.Scheduler,
        })
      } else if (_tags.Type === "Message") {
        await this.message({ item, process: item.target })
      } else {
        err = true
      }
      if (err) {
        res.status(400)
        res.send({ err })
      } else {
        res.send({ id: item.id })
      }
    })
    const server = app.listen(this.ports.mu, () =>
      console.log(`MU on port ${this.ports.mu}`),
    )
    this.servers.push(server)
  }
  su() {
    const app = express()
    app.use(cors())
    app.use(bodyParser.json())
    app.get("/", (req, res) => {
      res.json({
        Unit: "Scheduler",
        Timestamp: Date.now(),
        Address: su.addr,
        Processes: keys(this.mem.env),
      })
    })
    app.get("/timestamp", (req, res) =>
      res.json({ timestamp: Date.now(), block_height: this.mem.height }),
    )
    app.get("/:pid", (req, res) => {
      const pid = req.params.pid
      const edges = map(v => {
        const tx = this.mem.txs[v.id]
        const _tags = tags(v.tags)
        const mid = _tags.Message
        const mtx = this.mem.txs[mid]
        return {
          cursor: v.id,
          node: {
            message: {
              id: mtx.id,
              tags: mtx.tags,
              owner: this.mem.addrmap[mtx.owner],
              anchor: mtx.anchor ?? null,
              target: pid,
              signature: mtx.signature,
              data: tx.data,
            },
            assignment: {
              id: tx.id,
              tags: tx.tags,
              owner: this.mem.addrmap[tx.owner],
              anchor: tx.anchor ?? null,
              target: null,
              signature: tx.signature,
            },
          },
        }
      })(reverse(this.mem.env[pid].txs))
      res.json({ page_info: { has_next_page: false }, edges })
    })
    const server = app.listen(this.ports.su, () =>
      console.log(`SU on port ${this.ports.su}`),
    )
    this.servers.push(server)
  }
  cu() {
    const app = express()
    app.use(cors())
    app.use(bodyParser.json())
    app.get("/", (req, res) =>
      res.json({ timestamp: Date.now(), address: cu.addr }),
    )
    app.get("/result/:mid", async (req, res) => {
      const res2 = await this.result({
        message: req.params.mid,
        process: req.query["process-id"],
      })
      res.json(res2)
    })
    app.post("/dry-run", async (req, res) => {
      const process = req.query["process-id"]
      const { Id: id, Owner: owner, Tags: tags, Data: data } = req.body
      const res2 = await this.dryrun({ id, owner, tags, data, process })
      if (!res2) {
        res.status(400)
        res.json({ err: true })
      } else {
        delete res2.Memory
        res.json(res2)
      }
    })
    const server = app.listen(this.ports.cu, () =>
      console.log(`CU on port ${this.ports.cu}`),
    )
    this.servers.push(server)
  }
  end() {
    return new Promise(res => {
      let count = 0
      for (const v of this.servers)
        v.close(() => {
          count += 1
          if (count >= 4) {
            console.log("servers closed!", count)
            res()
          }
        })
    })
  }
}

export default Server
