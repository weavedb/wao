import use from "/lib/use"
import { Box, Flex } from "@chakra-ui/react"
import g from "/lib/global"
import { clone, map, addIndex, append } from "ramda"

export default function Left() {
  const [modules, setModules] = use("modules")
  const [message, setMessage] = use("message")
  const [tab, setTab] = use("tab")
  const [mod, setModule] = use("module")
  const [proc, setProc] = use("proc")
  const [messages, setMessages] = use("messages")
  const [procs, setProcs] = use("procs")

  const buttons = (
    <Flex
      h="50px"
      align="center"
      p={4}
      fontSize="12px"
      css={{ borderBottom: "1px solid #ddd" }}
    >
      <>
        <Flex
          py={1}
          px={3}
          fontSize="10px"
          color="#ddd"
          bg="#5137C5"
          css={{
            borderRadius: "5px",
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
          onClick={async () => {
            const jwk = await g.getWallet()
            //if (!jwk) return alert("wallet not connected")
            let pid, p
            ;({ pid, p } = await g.ao.deploy({ module: mod.id, jwk }))
            g.logSpawn(pid)
            g._setModule(mod.id)
            g._setProcess(pid)
            setTab("Processes")

            /*
            const v = pid
            let _proc = clone(g.ao.mem.env[v])
            delete _proc.memory
            _proc.tags = clone(g.ao.mem.msgs[v]?.tags ?? [])
            _proc.id = v
            setProc(_proc)
            setMessages(
              addIndex(map)((v, i) => {
                return {
                  id: v,
                  ...g.ao.mem.msgs[v],
                  slot: i,
                  http_msg: g.ao.mem.msgs[v],
                }
              })(_proc.results)
            )

            let mmap = {}
            for (let k in g.ao.mem.modules) mmap[g.ao.mem.modules[k]] = k
            setProcs(append({ txid: pid, module: mmap[_proc.module] }, procs))
            */
          }}
        >
          Spawn
        </Flex>
        <Box flex={1} />
      </>
    </Flex>
  )
  return (
    <Box
      fontSize="12px"
      w="315px"
      h="100%"
      css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
    >
      {buttons}
      {map(v => (
        <Flex
          h="50px"
          bg={v.txid === mod?.id ? "#5137C5" : "white"}
          fontSize="12px"
          p={4}
          direction="column"
          justify="center"
          onClick={() => g._setModule(v.txid)}
          css={{
            borderBottom: "1px solid #ddd",
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
        >
          <Box
            fontWeight="bold"
            color={v.txid !== mod?.id ? "#5137C5" : "#ddd"}
          >
            {v.name}
          </Box>
          <Box fontSize="10px" color={v.txid !== mod?.id ? "#222" : "#ddd"}>
            {v.txid}
          </Box>
        </Flex>
      ))(modules)}
    </Box>
  )
}
