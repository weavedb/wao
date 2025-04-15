import { is, map } from "ramda"
const guide = [
  "/",
  ["README"],
  "/tutorials/",
  [
    "README",
    "installation",
    "aoconnect-wrapper",
    "setup-project",
    "write-tests",
    "use-sdk",
    "cherrypick",
    "check-response",
    "async-receive",
    "logging",
    "fork-wasm-memory",
    "weavedrive",
    "local-server",
    "hyperbeam",
  ],
  "/api/",
  ["README", "ao", "process", "function-piping", "ar", "gql", "armem", "hb"],
]
let bfiles = []
let dir = null
for (let v of guide) {
  if (typeof v === "string") {
    if (bfiles.length > 0) {
      bfiles.push({
        dir: true,
        name: v.replace(/\//g, ""),
        id: v.replace(/\//g, ""),
        path: v.split("/").slice(0, -2).join("/") + "/",
        pid: "0",
      })
    }
    dir = v
    continue
  }
  if (is(Array, v)) {
    for (let v2 of v) {
      bfiles.push({
        ext: "md",
        name: `${v2}.md`,
        fetch: `/docs${dir}${v2}.md`,
        nodel: true,
        id: `docs${dir.replace(/\//g, "#")}${v2}`,
        path: dir,
        pid: "0",
      })
    }
  }
}
const bps = map(v => {
  return {
    ext: "lua",
    name: `${v}.lua`,
    fetch: `/blueprints/${v}.lua`,
    nodel: true,
    id: v,
    path: "/",
    pid: "3",
  }
})([
  "apm",
  "arena",
  "arns",
  "chat",
  "chatroom",
  "patch-legacy-reply",
  "staking",
  "token",
  "voting",
])

const default_projects = [
  { name: "Get Started", id: "0", open: true },
  { name: "Blueprints", id: "3", open: true },
  { name: "Default Project", id: "1", open: true },
]
export { bps, bfiles, default_projects }
