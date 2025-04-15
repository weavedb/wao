import { Box, Flex, Icon } from "@chakra-ui/react"
import { FaX } from "react-icons/fa6"

export default function Modal({ modal, setModal, children }) {
  return !modal ? null : (
    <Flex
      align="center"
      justify="center"
      css={{ position: "fixed", top: 0, left: 0, zIndex: 5 }}
      bg="rgba(1,1,1,0.5)"
      w="100vw"
      h="100vh"
    >
      <Box
        mb="50px"
        w="600px"
        bg="white"
        css={{ borderRadius: "10px", position: "relative" }}
      >
        <Flex justify="flex-end" w="100%" p={2} css={{ position: "absolute" }}>
          <Icon
            size="md"
            color="#999"
            m={2}
            css={{
              cursor: "pointer",
              _hover: { opacity: 0.75 },
            }}
            onClick={() => setModal(false)}
          >
            <FaX />
          </Icon>
        </Flex>
        {children}
      </Box>
    </Flex>
  )
}
