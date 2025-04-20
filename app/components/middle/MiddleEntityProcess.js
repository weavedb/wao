import { Box, Flex } from "@chakra-ui/react"
import { map, clone } from "ramda"
import g from "/lib/global"
import use from "/lib/use"
import { useState } from "react"
import { short, fromNow } from "/lib/utils"
import Tags from "/components/Tags"

export default function MiddleEntityProcess() {
  const [subtab, setSubtab] = useState("Metadata")
  const [entity, setEntity] = use("entity")
  const [terminal, setTerminal] = use("terminal")

  const subtabs = ["Metadata", "Incoming", "Outgoing"]
  const buttons = !entity ? null : (
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
        <Box>
          {entity.type[0].toUpperCase()}
          {entity.type.slice(1)} / {entity.id}
        </Box>
      </Flex>
      <Flex h="30px" align="center">
        {map(v => {
          return (
            <Flex
              fontSize="12px"
              h="100%"
              px={3}
              onClick={() => setSubtab(v)}
              css={{
                textDecoration: v === subtab ? "underline" : "",
                cursor: "pointer",
                _hover: { opacity: 0.75 },
              }}
              color={v === subtab ? "#5137c5" : ""}
              align="center"
            >
              {v}
            </Flex>
          )
        })(subtabs)}
        <Box flex={1} />
        <Flex
          mr={4}
          py={1}
          px={4}
          fontSize="10px"
          color="#ddd"
          bg="#5137C5"
          css={{
            borderRadius: "5px",
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
          onClick={async () => {
            const p = g.ao.p(entity.id)
            const lua = g.editorRef.current.getValue()
            const jwk = await g.getWallet()
            //if (!jwk) return alert("wallet not connected")
            const res = await p.msg("Eval", { data: lua, jwk })
            g.logMsg(res.mid)
            g.addMsg(res.mid)
          }}
        >
          Eval
        </Flex>
        {terminal === entity.id ? (
          <Flex
            mr={4}
            py={1}
            px={4}
            fontSize="10px"
            bg="white"
            color="#5137C5"
            css={{
              border: "1px solid #5317c5",
              borderRadius: "5px",
            }}
          >
            Connected
          </Flex>
        ) : (
          <Flex
            mr={4}
            py={1}
            px={4}
            fontSize="10px"
            color="#ddd"
            bg="#5137C5"
            css={{
              borderRadius: "5px",
              cursor: "pointer",
              _hover: { opacity: 0.75 },
            }}
            onClick={async () => {
              g._setProcess(entity.id)
            }}
          >
            Connect
          </Flex>
        )}
      </Flex>
    </Flex>
  )
  let meta = []
  if (entity) {
    meta.push({ name: "ID", value: entity.id })
    meta.push({ name: "Module", value: entity.module, link: true })
    meta.push({ name: "Owner", value: entity.owner, link: true })
    meta.push({ name: "Timestamp", value: fromNow(entity.timestamp) })
  }
  return (
    <Box w="100%" h="100%">
      {buttons}
      {subtab === "Metadata" ? (
        <Flex h="100%">
          <Box fontSize="10px" w="315px" py={2}>
            {map(v => {
              return (
                <Flex p={4} direction="column" h="40px" justify="center" mb={2}>
                  <Box fontWeight="bold" color="#5137c5" fontSize="12px" mb={1}>
                    {v.name}
                  </Box>
                  <Box
                    css={{
                      cursor: v.link ? "pointer" : "default",
                      _hover: { opacity: v.link ? 0.75 : 1 },
                    }}
                    onClick={() => {
                      if (v.link) g.getAccount(v.value)
                    }}
                  >
                    {v.value}
                  </Box>
                </Flex>
              )
            })(meta)}
          </Box>
          <Box
            flex={1}
            p={4}
            fontSize="11px"
            css={{ borderLeft: "1px solid #ddd", overflowY: "auto" }}
            h="calc(100vh - 120px)"
          >
            <Tags tags={entity.tags} />
          </Box>
        </Flex>
      ) : subtab === "Incoming" ? (
        <>
          <Flex h="30px" bg="#eee" align="center" fontSize="10px">
            <Box px={3} fontSize="10px" w="80px">
              Slot
            </Box>
            <Box px={3} fontSize="10px" w="120px">
              Action
            </Box>
            <Box px={3} fontSize="10px" w="300px">
              TxID
            </Box>
            <Box px={3} fontSize="10px" w="150px">
              From
            </Box>
            <Box px={3} fontSize="10px" flex={1}>
              Timestamp
            </Box>
          </Flex>

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
              onClick={() => g.getMessage(v.id)}
            >
              <Box px={3} fontSize="10px" w="80px">
                {v.slot}
              </Box>
              <Box px={3} fontSize="10px" w="120px">
                {v.act}
              </Box>
              <Box px={3} fontSize="10px" w="300px">
                {v.id}
              </Box>
              <Box px={3} fontSize="10px" w="150px">
                {short(v.from)}
              </Box>
              <Box px={3} fontSize="10px" flex={1}>
                {fromNow(v.timestamp)}
              </Box>
            </Flex>
          ))(entity.incoming || [])}
        </>
      ) : subtab === "Outgoing" ? (
        <>
          <Flex h="30px" bg="#eee" align="center" fontSize="10px">
            <Box px={3} fontSize="10px" w="120px">
              Action
            </Box>
            <Box px={3} fontSize="10px" w="300px">
              TxID
            </Box>
            <Box px={3} fontSize="10px" w="150px">
              To
            </Box>
            <Box px={3} fontSize="10px" flex={1}>
              Timestamp
            </Box>
          </Flex>

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
              onClick={() => g.getMessage(v.id)}
            >
              <Box px={3} fontSize="10px" w="120px">
                {v.act}
              </Box>
              <Box px={3} fontSize="10px" w="300px">
                {v.id}
              </Box>
              <Box px={3} fontSize="10px" w="150px">
                {short(v.to)}
              </Box>

              <Box px={3} fontSize="10px" flex={1}>
                {fromNow(v.timestamp)}
              </Box>
            </Flex>
          ))(entity.outgoing || [])}
        </>
      ) : null}
    </Box>
  )
}
