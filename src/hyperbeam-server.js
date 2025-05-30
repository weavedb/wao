import express from "express"
import cors from "cors"
import bodyParser from "body-parser"

const run = (port = 4000) => {
  const app = express()
  app.use(cors())
  app.use(
    express.raw({
      type: "application/octet-stream",
      limit: "10mb",
    })
  )
  app.use(express.json())
  app.post("/relay", async (req, res) => {
    res.json({ success: true, body: req.body.toString() })
  })
  return app.listen(port, () => console.log(`Hyper WAO on port ${port}`))
}

export { run }
