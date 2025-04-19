import { Box, Flex } from "@chakra-ui/react"
import { map } from "ramda"
import g from "/lib/global"

const isaddr = /^[A-Za-z0-9_-]{43}$/
export default function Tags({ tags }) {
  return (
    <>
      <Flex mb={2} fontWeight="bold" color="#5137C5" fontSize="14px">
        Tags
      </Flex>
      {map(v => {
        const addr = isaddr.test(v.value)
        return (
          <Flex my={2} align="center">
            <Box
              w="130px"
              color="white"
              bg="#5137C5"
              px={2}
              mr={4}
              css={{
                borderRadius: "3px",
              }}
            >
              {v.name}
            </Box>
            <Box
              onClick={() => addr && g.getAccount(v.value)}
              css={{
                _hover: { opacity: addr ? 0.75 : 1 },
                cursor: addr ? "pointer" : "default",
              }}
            >
              {v.value}
            </Box>
          </Flex>
        )
      })(tags)}
    </>
  )
}
