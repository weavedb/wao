import { Box, Flex } from "@chakra-ui/react"
import { Spinner } from "@chakra-ui/react"
export default function Logo() {
  return (
    <Flex
      w="100%"
      h="100%"
      align="center"
      justify="center"
      fontSize="30px"
      bg="#5137C5"
      color="#9C89F6"
      direction="column"
      css={{ position: "fixed", top: 0, left: 0, zIndex: 100 }}
    >
      <Flex css={{ position: "relative" }} align="center" justify="center">
        <Spinner
          boxSize="350px"
          css={{ position: "absolute" }}
          borderWidth="5px"
          animationDuration="1s"
        />
        <Box
          boxSize="300px"
          css={{
            backgroundImage: "url(/logo.png)",
            backgroundSize: "cover",
            backgroundPosition: "center",
          }}
        />
      </Flex>
    </Flex>
  )
}
