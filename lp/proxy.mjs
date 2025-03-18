import express from "express"
import { createProxyMiddleware } from "http-proxy-middleware"

const app = express()

const port = (process.argv[2] ?? "4000") * 1
const target = process.argv[3] ?? "http://localhost:3000"
const sub = process.argv[4] ?? "7J-cq6dcJAz0zkzcrRNb-2FTZRuQXIij1P1FiaQxdaE"

app.use(
  `/${sub}`,
  createProxyMiddleware({
    target,
    changeOrigin: true,
    pathRewrite: { "^/arweave": "" },
  }),
)

app.listen(port, () => {
  console.log(`Proxy server running at http://localhost:${port}/${sub}`)
})
