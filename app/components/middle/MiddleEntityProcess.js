import { Box, Flex } from "@chakra-ui/react"
import { map, clone } from "ramda"
import g from "/lib/global"
import use from "/lib/use"
import { useState } from "react"
import { fromNow } from "/lib/utils"

export default function MiddleEntityProcess() {
  const [subtab, setSubtab] = useState("Metadata")
  const [entity, setEntity] = use("entity")
  const subtabs = ["Metadata", "Incoming"]
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
    meta.push({ name: "Module", value: entity.module })
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
              <Box px={3} fontSize="10px" flex={1}>
                {fromNow(v.timestamp)}
              </Box>
            </Flex>
          ))(entity.incoming || [])}
        </>
      ) : null}
    </Box>
  )
}
