import { Box, Flex } from "@chakra-ui/react"
import { map, clone } from "ramda"
import g from "/lib/global"
import use from "/lib/use"
import { useState } from "react"
import { short, fromNow } from "/lib/utils"

export default function MiddleEntityProcess() {
  const [subtab, setSubtab] = useState("Metadata")
  const [entity, setEntity] = use("entity")
  const subtabs = ["Metadata", "Outgoing", "Processes"]
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
              Tokens
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
                  {v.ticker}
                </Box>
                <Box>{v.balance}</Box>
              </Flex>
            ))(entity?.tokens || [])}
          </Box>
        </Flex>
      ) : subtab === "Outgoing" ? (
        <>
          <Flex h="30px" bg="#eee" align="center" fontSize="10px">
            <Box px={3} fontSize="10px" w="120px">
              Action
            </Box>
            <Box px={3} fontSize="10px" w="300px">
              ID
            </Box>
            <Box px={3} fontSize="10px" w="150px">
              From
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
                {short(v.from)}
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
      ) : subtab === "Processes" ? (
        <>
          <Flex h="30px" bg="#eee" align="center" fontSize="10px">
            <Box px={3} w="120px" _groupHover={{ color: "white" }}>
              Name
            </Box>
            <Box px={3} fontSize="10px" w="300px">
              ID
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
                {fromNow(v.timestamp)}
              </Box>
            </Flex>
          ))(entity.spawn || [])}
        </>
      ) : null}
    </Box>
  )
}
