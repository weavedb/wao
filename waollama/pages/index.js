import { Link, ssr } from "arnext"
import { Image, Box, Flex, Textarea } from "@chakra-ui/react"
import { useEffect, useState } from "react"
import { reverse, append, map } from "ramda"
import wasm from "../lib/wasm"
//import { AO, acc } from "../../src/web"
import { AO, acc } from "wao/web"
import client from "../lib/client"
const getDate = async date => date ?? Date.now()

export const getStaticProps = ssr(async ({}) => {
  return { props: { _date: Date.now() }, revalidate: 100 }
})

const genMsg = async (Id, p, data, Tags, from, Owner, dry = false) => {
  if (!dry) p.height += 1
  return {
    Id,
    Target: p.id,
    Owner,
    Data: data?.length ? data : "",
    "Block-Height": "1",
    Timestamp: Date.now().toString(),
    Module: p.module,
    From: from,
    Cron: false,
    Tags: Tags?.length ? Tags : [],
  }
}

const genEnv = async ({ pid, owner = "", module = "" }) => {
  return {
    Process: {
      Id: pid,
      Tags: [],
      Owner: owner,
    },
    Module: {
      Id: module,
      Tags: [],
    },
  }
}

const src_data = `
Llama = require(".Llama")
Llama.logLevel = 4

Handlers.add("Load", "Load", function (msg)
  Llama.load("/data/" .. msg.ModelID)
  msg.reply({ Data = "ok" })
end)

Handlers.add("Ask", "Ask", function (msg)
  Llama.setPrompt(msg.Q)
  msg.reply({ Data = Llama.run(30) })
end)
`

let llm = null
let once = false
export default function Home({ _date = null }) {
  const [init, setInit] = useState(false)
  const [deploying, setDeploying] = useState(false)
  const [sending, setSending] = useState(false)
  const [msg, setMsg] = useState("")
  const [reply, setReply] = useState([])
  useEffect(() => {
    ;(async () => {
      if (once) return
      once = true
    })()
  }, [])

  return (
    <Flex align="center" justify="center" p={6} h="100vh">
      <Box w="100%" maxW="700px">
        <Box
          fontSize="30px"
          fontWeight="bold"
          textAlign="center"
          color="#5137C5"
        >
          WaoLlama
        </Box>
        <Box fontSize="16px" p={4} textAlign="center" color="#5137C5">
          <Box>
            WaoLlama is a fully decentralized autonomous agent running on WAO.
          </Box>
          <Box>
            AO units & LLMs are fully embedded in your browser. No external
            dependencies.
          </Box>
        </Box>
        {!init ? (
          <Flex justify="center">
            <Box
              onClick={async () => {
                if (deploying) return
                setDeploying(true)
                const ao = await new AO().init(acc[0])
                await ao.mem.init()
                for (let k in ao.mem.env) {
                  llm = ao.p(k)
                  break
                }
                if (llm) setInit(true)
                else {
                  const _module = await fetch("/llama/llama.wasm").then(r =>
                    r.arrayBuffer()
                  )
                  const { id: modid } = await ao.postModule({
                    data: Buffer.from(_module),
                  })
                  console.log("module:", modid)
                  const model = await fetch("/llama/tinyllama.gguf").then(r =>
                    r.arrayBuffer()
                  )

                  const { id } = await ao.ar.post({
                    data: Buffer.from(model),
                    tags: { "Memory-Limit": "2-gb" },
                  })
                  console.log("model:", id)

                  const { p, pid, err } = await ao.deploy({
                    tags: { Extension: "WeaveDrive", Attestor: ao.ar.addr },
                    module: modid,
                    src_data,
                  })
                  llm = p
                  console.log("pid", pid, err)
                  console.log("attested:", await ao.attest({ id }))
                  if ((await p.m("Load", { ModelID: id }, false)) === "ok") {
                    setInit(true)
                  }
                }
                setDeploying(false)
              }}
              px={6}
              py={2}
              color="#5137C5"
              fontSize="24px"
              mt={2}
              css={{
                border: "1px solid #5137C5",
                borderRadius: "3px",
                cursor: "pointer",
                _hover: { opacity: 0.75 },
              }}
            >
              {deploying ? "Initializing..." : "Deploy Agent on AO"}
            </Box>
          </Flex>
        ) : (
          <div>
            <Textarea
              css={{ border: "1px solid #5137C5" }}
              value={msg}
              onChange={e => setMsg(e.target.value)}
            />
            <Flex justify="flex-end">
              <Box
                my={2}
                px={4}
                py={1}
                color="#5137C5"
                css={{
                  border: "1px solid #5137C5",
                  borderRadius: "3px",
                  cursor: "pointer",
                  _hover: { opacity: 0.75 },
                }}
                onClick={async () => {
                  if (sending) return
                  setSending(true)
                  setMsg("")
                  let _reply = append({ who: "you", msg }, reply)
                  setReply(_reply)
                  const rep = await llm.d("Ask", { Q: msg }, false)
                  console.log(rep)
                  setReply(append({ who: "agent", msg: rep }, _reply))
                  setSending(false)
                }}
              >
                {sending ? "Sending..." : "Send"}
              </Box>
            </Flex>
            <div>
              {map(v => (
                <Flex my={6}>
                  <Image
                    boxSize="50px"
                    src={v.who === "you" ? `human.png` : "llama.png"}
                  />
                  <Box
                    color="#5137C5"
                    ml={4}
                    p={4}
                    css={{ borderRadius: "5px", border: "1px solid #5137C5" }}
                  >
                    {v.msg}
                  </Box>
                </Flex>
              ))(reverse(reply))}
            </div>
          </div>
        )}
      </Box>
    </Flex>
  )
}
