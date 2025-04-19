import { Box, Flex } from "@chakra-ui/react"
import { Bundle } from "arbundles"
import { map, clone } from "ramda"
import g from "/lib/global"
import use from "/lib/use"
import { useState } from "react"
import { tags, short, fromNow } from "/lib/utils"

export default function MiddleEntityTx() {
  const [subtab, setSubtab] = useState("Metadata")
  const [entity, setEntity] = use("entity")
  const subtabs = ["Metadata", "DataItems"]
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
  const t = tags(entity.tags)
  let isBundle = false
  let items = []
  if (t["Bundle-Format"] === "binary" && t["Bundle-Version"] === "2.0.0") {
    isBundle = true
    const bd = new Bundle(entity.data)
    for (let v of bd.items) {
      const id = v.id
      const t = tags(v.tags)
      items.push({ id, type: t.Type, to: v.target, act: t.Action })
    }
  }
  let meta = []
  if (entity) {
    meta.push({ name: "ID", value: entity.id })
    meta.push({ name: "Owner", value: entity.owner, link: true })
    meta.push({ name: "Target", value: entity.target, link: true })
    meta.push({ name: "Data Size", value: entity.data_size + " B" })
    meta.push({ name: "Bundled DataItems", value: items.length })
    meta.push({ name: "Quantity", value: entity.quantity })
    meta.push({ name: "Block", value: entity.block, link: true, block: true })
    meta.push({ name: "Timestamp", value: fromNow(entity.timestamp) })
  }
  console.log(entity)
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
                      if (v.block) g.getBlock(v.value)
                      else if (v.link) g.getAccount(v.value)
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
            h="calc(100vh - 120px)"
            css={{ borderLeft: "1px solid #ddd", overflowY: "auto" }}
          >
            <Flex mb={2} fontWeight="bold" color="#5137C5" fontSize="14px">
              Tags
            </Flex>
            {map(v => (
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
                <Box>{v.value}</Box>
              </Flex>
            ))(entity.tags)}
            <Flex
              mt={4}
              fontWeight="bold"
              mb={2}
              color="#5137C5"
              fontSize="14px"
            >
              Signature
            </Flex>
            <code>
              <Box
                as="pre"
                bg="#eee"
                p={4}
                css={{
                  borderRadius: "3px",
                  wordBreak: "break-all",
                  whiteSpace: "pre-wrap",
                  overflow: "auto",
                }}
              >
                {entity.signature}
              </Box>
            </code>
          </Box>
        </Flex>
      ) : subtab === "DataItems" ? (
        <>
          <Flex h="30px" bg="#eee" align="center" fontSize="10px">
            <Box px={3} w="120px" _groupHover={{ color: "white" }}>
              Type
            </Box>
            <Box px={3} w="120px" _groupHover={{ color: "white" }}>
              Action
            </Box>
            <Box px={3} fontSize="10px" w="300px">
              ID
            </Box>
            <Box px={3} fontSize="10px" w="150px">
              To
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
                g.getAccount(v.id)
              }}
            >
              <Box px={3} w="120px" _groupHover={{ color: "white" }}>
                {v.type}
              </Box>
              <Box px={3} w="120px" _groupHover={{ color: "white" }}>
                {v.act}
              </Box>

              <Box px={3} fontSize="10px" w="300px">
                {v.id}
              </Box>
              <Box px={3} fontSize="10px" w="150px">
                {short(v.to)}
              </Box>
            </Flex>
          ))(items)}
        </>
      ) : null}
    </Box>
  )
}
