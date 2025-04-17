import use from "/lib/use"
import { Box, Flex } from "@chakra-ui/react"
import g from "/lib/global"
import { map, clone } from "ramda"

export default function Left() {
  const [file, setFile] = use("file")
  const [tab, setTab] = use("tab")
  const [proc, setProc] = use("proc")
  const [procs, setProcs] = use("procs")
  const [message, setMessage] = use("message")
  const buttons = (
    <Flex
      h="50px"
      align="center"
      p={4}
      fontSize="12px"
      css={{ borderBottom: "1px solid #ddd" }}
    >
      {!proc || !file || file.ext !== "lua" ? null : (
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
              if (!proc) {
                alert("Select a processl")
              } else {
                const p = g.ao.p(proc.id)
                const lua = g.editorRef.current.getValue()
                const jwk = await g.getWallet()
                //if (!jwk) return alert("wallet not connected")
                const res = await p.msg("Eval", { data: lua, jwk })
                g.logMsg(res.mid)
                g.addMsg(res.mid)
              }
            }}
          >
            Eval
          </Flex>
          <Box flex={1} />
        </>
      )}
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
          bg={v.txid === proc?.id ? "#5137C5" : "white"}
          fontSize="12px"
          p={4}
          direction="column"
          justify="center"
          onClick={() => {
            let _proc = clone(g.ao.mem.env[v.txid])
            delete _proc.memory
            _proc.tags = clone(g.ao.mem.msgs[v.txid]?.tags ?? [])
            _proc.id = v.txid
            setProc(_proc)
            setMessage(null)
          }}
          css={{
            borderBottom: "1px solid #ddd",
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
        >
          <Box
            fontWeight="bold"
            color={v.txid !== proc?.id ? "#5137C5" : "#ddd"}
          >
            {v.module}
          </Box>
          <Box fontSize="10px" color={v.txid !== proc?.id ? "#222" : "#ddd"}>
            {v.txid}
          </Box>
        </Flex>
      ))(procs)}
    </Box>
  )
}
