import { Box, Flex } from "@chakra-ui/react"
import { map, clone } from "ramda"
import g from "/lib/global"
import use from "/lib/use"
import { useState } from "react"
import { short, fromNow } from "/lib/utils"

export default function MiddleEntityBlock() {
  const [subtab, setSubtab] = useState("Metadata")
  const [entity, setEntity] = use("entity")
  const subtabs = ["Metadata"]
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
      </Flex>
    </Flex>
  )
  let meta = []
  if (entity) {
    meta.push({ name: "ID", value: entity.id })
    meta.push({ name: "Height", value: entity.height })
    meta.push({ name: "Previous Block", value: entity.previous, link: true })
    meta.push({ name: "Transactions", value: entity.tx_count })
    meta.push({ name: "Messages", value: entity.msg_count })
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
            fontSize="11px"
            css={{ borderLeft: "1px solid #ddd", overflowY: "auto" }}
            h="calc(100vh - 120px)"
          >
            {map(v => (
              <Flex direction="column" css={{ borderBottom: "1px solid #ddd" }}>
                <Flex
                  fontSize="10px"
                  h="30px"
                  align="center"
                  css={{
                    cursor: "pointer",
                    _hover: { color: "#ddd", bg: "#5137C5" },
                  }}
                  className="group"
                  onClick={() => g.getAccount(v.id)}
                  bg="#eee"
                >
                  <Flex px={3} w="120px">
                    {v.type === "bundle" ? "Bundle" : "Tx"}
                  </Flex>
                  <Box px={3} fontSize="10px" w="300px">
                    {v.id}
                  </Box>
                </Flex>
                {map(v2 => (
                  <Flex
                    fontSize="10px"
                    h="30px"
                    align="center"
                    css={{
                      cursor: "pointer",
                      _hover: { color: "#ddd", bg: "#5137C5" },
                      borderTop: "1px solid #ddd",
                    }}
                    className="group"
                    onClick={() => g.getAccount(v2.id)}
                  >
                    <Flex px={3} w="120px">
                      {v2.type}
                    </Flex>
                    <Box px={3} fontSize="10px" w="300px">
                      {v2.id}
                    </Box>
                  </Flex>
                ))(v.txs)}
              </Flex>
            ))(entity.transactions || [])}
          </Box>
        </Flex>
      ) : null}
    </Box>
  )
}
