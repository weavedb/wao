import { Box, Flex } from "@chakra-ui/react"
import { Icon } from "@chakra-ui/react"
import { FaBookOpen, FaDiscord, FaXTwitter, FaGithub } from "react-icons/fa6"
import use from "/lib/use"

export default function Footer() {
  const [wasm64] = use("wasm64")
  return (
    <Flex
      w="100%"
      h="30px"
      align="center"
      px={4}
      bg="white"
      fontSize="12px"
      color="#666"
      css={{ borderTop: "1px solid #ddd" }}
    >
      <Box w="200px">LOCALNET v 1.0.0</Box>
      {!wasm64 ? (
        <Flex justify="center" flex={1}>
          Only possible on Arweave and AO
        </Flex>
      ) : (
        <Flex justify="center" flex={1} color="#FF4136">
          Wasm64 is unsupported with your browser
        </Flex>
      )}

      <Flex justify="flex-end" w="200px">
        <Box
          mr={3}
          as="a"
          target="_blank"
          href="https://github.com/weavedb/wao"
        >
          <Icon
            size="md"
            color="#666"
            css={{ cursor: "pointer", _hover: { opacity: 0.75 } }}
          >
            <FaGithub />
          </Icon>
        </Box>

        <Box mr={3} as="a" target="_blank" href="https://x.com/waoeco">
          <Icon
            size="md"
            color="#666"
            css={{ cursor: "pointer", _hover: { opacity: 0.75 } }}
          >
            <FaXTwitter />
          </Icon>
        </Box>
        <Box mr={3} as="a" target="_blank" href="https://discord.gg/vCkuVhkugY">
          <Icon
            size="md"
            color="#666"
            css={{ cursor: "pointer", _hover: { opacity: 0.75 } }}
          >
            <FaDiscord />
          </Icon>
        </Box>
        <Box as="a" target="_blank" href="https://docs.wao.eco">
          <Icon
            size="md"
            color="#666"
            css={{ cursor: "pointer", _hover: { opacity: 0.75 } }}
          >
            <FaBookOpen />
          </Icon>
        </Box>
      </Flex>
    </Flex>
  )
}
