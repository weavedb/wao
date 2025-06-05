import express from "express"
import cors from "cors"
import bodyParser from "body-parser"

const run = (port = 4000) => {
  const app = express()
  app.use(cors())
  app.use(bodyParser.json({ limit: "100mb" }))
  app.use(bodyParser.raw({ type: "application/octet-stream", limit: "100mb" }))
  app.use((req, res) => {
    console.log({
      method: req.method,
      path: req.path,
      url: req.url,
      body: req.body,
    })

    // Handle based on method
    if (req.method === "GET") {
      res.send("ok - GET")
    } else if (req.method === "POST") {
      res.send("ok - POST")
    } else {
      res.send(`ok - ${req.method}`)
    }
  })
  app.listen(port, () => console.log(`gateway on port ${port}`))
}

export default run
