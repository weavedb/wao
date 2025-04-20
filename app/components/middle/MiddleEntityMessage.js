import { Box, Flex } from "@chakra-ui/react"
import { map, clone } from "ramda"
import g from "/lib/global"
import use from "/lib/use"
import { useEffect, useState } from "react"
import { short, fromNow } from "/lib/utils"
import Tags from "/components/Tags"

export default function MiddleEntityMessage() {
  const [subtab, setSubtab] = useState("Metadata")
  const [entity, setEntity] = use("entity")
  const subtabs = ["Metadata", "Resulted", "Linked"]
  useEffect(() => {
    setSubtab("Metadata")
  }, [entity])
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
    meta.push({ name: "Process", value: entity.process, link: true })
    meta.push({ name: "From", value: entity.from, link: true })
    meta.push({ name: "To", value: entity.to, link: true })
    meta.push({ name: "Timestamp", value: fromNow(entity.timestamp) })
  }
  return (
    <Box w="100%" h="100%">
      {buttons}
      {subtab === "Resulted" ? (
        <>
          <Flex h="30px" bg="#eee" align="center" fontSize="10px">
            <Box px={3} w="120px" _groupHover={{ color: "white" }}>
              Type
            </Box>
            <Box px={3} w="120px" _groupHover={{ color: "white" }}>
              Action
            </Box>
            <Box px={3} w="300px">
              ID
            </Box>
            <Box px={3} w="150px">
              From
            </Box>
            <Box px={3} w="150px">
              To
            </Box>
            <Box px={3} flex={1}>
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
                {short(v.from)}
              </Box>
              <Box px={3} fontSize="10px" w="150px">
                {short(v.to)}
              </Box>
              <Box px={3} fontSize="10px" flex={1}>
                {fromNow(v.timestamp)}
              </Box>
            </Flex>
          ))(entity.resulted || [])}
        </>
      ) : subtab === "Metadata" ? (
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
            h="calc(100vh - 120px)"
            css={{ borderLeft: "1px solid #ddd", overflowY: "auto" }}
          >
            <Tags tags={entity.tags} />
            <Flex
              mt={4}
              fontWeight="bold"
              mb={2}
              color="#5137C5"
              fontSize="14px"
            >
              Data
            </Flex>
            <code>
              <Box
                as="pre"
                bg="#eee"
                p={4}
                css={{
                  borderRadius: "3px",
                  wordBreak: "break-word",
                  whiteSpace: "pre-wrap",
                  overflow: "auto",
                }}
              >
                {entity.data}
              </Box>
            </code>

            <Flex
              mt={4}
              mb={2}
              fontWeight="bold"
              color="#5137C5"
              fontSize="14px"
            >
              Result
            </Flex>
            <code>
              <Box
                as="pre"
                bg="#eee"
                p={4}
                css={{
                  borderRadius: "3px",
                  wordBreak: "break-word",
                  whiteSpace: "pre-wrap",
                  overflow: "auto",
                }}
              >
                {!entity.process
                  ? "This message was sent to a non process account."
                  : !entity.res
                    ? ""
                    : JSON.stringify(entity.res, undefined, 2)}
              </Box>
            </code>
          </Box>
        </Flex>
      ) : null}
    </Box>
  )
}
