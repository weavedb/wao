import { Box, Flex } from "@chakra-ui/react"
import { map, clone } from "ramda"
import g from "/lib/global"
import use from "/lib/use"
import { useState } from "react"
import { fromNow } from "/lib/utils"
import Tags from "/components/Tags"

export default function MiddleEntityModule() {
  const [subtab, setSubtab] = useState("Metadata")
  const [entity, setEntity] = use("entity")
  const subtabs = ["Metadata", "Processes"]
  const [tab, setTab] = use("tab")
  const [wasm64] = use("wasm64")
  let iswasm = false
  try {
    iswasm =
      g.ao.mem.wasms[entity.id]?.format.split("-")[0] !== "wasm64" || wasm64
  } catch (e) {}
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
          py={1}
          px={3}
          mx={3}
          fontSize="10px"
          color="#ddd"
          bg={!iswasm ? "#999" : "#5137C5"}
          css={{
            borderRadius: "5px",
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
          onClick={async () => {
            if (!iswasm) {
              alert("Wasm64 modules are not supported with your browser.")
            } else {
              const jwk = await g.getWallet()
              const { pid, p } = await g.ao.deploy({ module: entity.id, jwk })
              g.logSpawn(pid)
              g._setModule(entity.id)
              g._setProcess(pid)
              setTab("Processes")
            }
          }}
        >
          Spawn
        </Flex>
      </Flex>
    </Flex>
  )
  let meta = []
  if (entity) {
    meta.push({ name: "TxID", value: entity.id })
    meta.push({ name: "Name", value: entity.name })
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
                  <Box>{v.value}</Box>
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
      ) : (
        <>
          <Flex h="30px" bg="#eee" align="center" fontSize="10px">
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
              Outgoing
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
                {v.outgoing ?? 0}
              </Box>
              <Box px={3} fontSize="10px" flex={1}>
                {fromNow(v.timestamp)}
              </Box>
            </Flex>
          ))(entity.processes)}
        </>
      )}
    </Box>
  )
}
