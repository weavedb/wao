import { useState } from "react"
import { Box, Flex } from "@chakra-ui/react"
import { map } from "ramda"
import g from "/lib/global"
import use from "/lib/use"

export default function MiddleProcesses() {
  const [procs, setProcs] = use("procs")
  const [subtab, setSubtab] = useState("Tags")
  const subtabs = ["Tags", "Processes"]
  const buttons = (
    <Flex
      h="60px"
      justify="center"
      fontSize="10px"
      css={{ borderBottom: "1px solid #ddd" }}
      direction="column"
    >
      <Flex
        fontSize="12px"
        h="30px"
        px={3}
        align="center"
        bg="#5137c5"
        color="#ddd"
        css={{ borderBottom: "1px solid #ddd" }}
      >
        <Box>Processes / {procs.length}</Box>
      </Flex>
      <Flex h="30px" bg="#eee" align="center">
        <Box px={3} w="120px" _groupHover={{ color: "white" }}>
          Name
        </Box>
        <Box px={3} fontSize="10px" w="300px">
          TxID
        </Box>
        <Box px={3} fontSize="10px" w="120px">
          Module
        </Box>
        <Box px={3} fontSize="10px" w="80px">
          Messages
        </Box>
        <Box px={3} fontSize="10px" flex={1}>
          Timestamp
        </Box>
      </Flex>
    </Flex>
  )
  return (
    <Box w="100%">
      {buttons}
      {map(v => (
        <Flex
          fontSize="10px"
          h="30px"
          align="center"
          css={{
            borderBottom: "1px solid #ddd",
            cursor: "pointer",
            _hover: { color: "#ddd", bg: "#5137C5" },
          }}
          className="group"
          onClick={() => {
            g.getProcess(v.id)
          }}
        >
          <Box px={3} w="120px" _groupHover={{ color: "white" }}>
            {v.name}
          </Box>
          <Box px={3} fontSize="10px" w="300px">
            {v.id}
          </Box>
          <Box px={3} fontSize="10px" w="120px">
            {v.module}
          </Box>
          <Box px={3} fontSize="10px" w="80px">
            {v.incoming}
          </Box>
          <Box px={3} fontSize="10px" flex={1}>
            {v.timestamp}
          </Box>
        </Flex>
      ))(procs || [])}
    </Box>
  )
}

function MiddleOld() {
  const [message, setMessage] = use("message")
  const [init, setInit] = use("init")
  const [tab, setTab] = use("tab")
  const [proc, setProc] = use("proc")
  const [messages, setMessages] = use("messages")

  let pmeta = []
  if (proc) {
    pmeta.push({ name: "ID", value: proc.id })
    pmeta.push({ name: "Owner", value: proc.owner })
  }

  return !proc ? null : (
    <Box
      px={4}
      py={2}
      fontSize="11px"
      flex={1}
      h="calc(100vh - 110px)"
      css={{ overflowY: "auto" }}
    >
      <Flex my={2} fontWeight="bold" color="#5137C5">
        Metadata
      </Flex>
      {map(v => {
        if (includes(v.name, ["signature", "signature-input"])) {
          return null
        }
        return (
          <Flex my={2} align="center">
            <Box
              w="130px"
              color="white"
              bg="#5137C5"
              px={2}
              mr={4}
              css={{ borderRadius: "3px" }}
            >
              {v.name}
            </Box>
            <Box flex={1} css={{ wordBreak: "break-all", whiteSpace: "wrap" }}>
              {v.value}
            </Box>
          </Flex>
        )
      })(pmeta ?? [])}
      <Flex my={2} fontWeight="bold" color="#5137C5">
        Tags
      </Flex>
      {map(v => {
        if (includes(v.name, ["signature", "signature-input"])) {
          return null
        }
        return (
          <Flex my={2} align="center">
            <Box
              w="130px"
              color="white"
              bg="#5137C5"
              px={2}
              mr={4}
              css={{ borderRadius: "3px" }}
            >
              {v.name}
            </Box>
            <Box flex={1} css={{ wordBreak: "break-all" }}>
              {v.value}
            </Box>
          </Flex>
        )
      })(proc?.tags || [])}
      <Flex mt={4} mb={2} fontWeight="bold" color="#5137C5">
        Messages ( {proc.results.length} )
      </Flex>
      {map(v => (
        <Flex
          py={2}
          align="center"
          css={{
            borderBottom: "1px solid #ddd",
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
          onClick={() => {
            let _msg = clone(g.ao.mem.msgs[v.id])
            _msg.id = v.id
            if (_msg.http_msg) setMessage(_msg)
            else {
              if (!_msg.tags) {
                _msg.tags = new DataItem(_msg.item.binary).tags
              }
              setMessage({
                res: _msg.res,
                http_msg: _msg,
                id: _msg.id,
                slot: v.slot,
              })
            }
            setTab("Messages")
          }}
        >
          <Flex color="#222" align="center">
            <Flex
              w="30px"
              mr={4}
              align="center"
              fontSize="10px"
              bg="#ddd"
              justify="center"
              css={{ borderRadius: "3px" }}
            >
              {v.slot}
            </Flex>
            <Box>{v.id}</Box>
          </Flex>
        </Flex>
      ))(messages)}
    </Box>
  )
}
