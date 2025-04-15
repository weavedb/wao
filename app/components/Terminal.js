import { Box, Flex } from "@chakra-ui/react"
import { map } from "ramda"
import dynamic from "next/dynamic"
import { dayjs } from "/lib/utils"
const XTerminal = dynamic(
  () => import("../components/Xterm").then(mod => mod),
  {
    ssr: false,
  }
)

export default function Terminal({
  ttab,
  global,
  ttabs,
  setTtab,
  setDryrun,
  dryrun,
  logs,
  containerRef,
}) {
  const terminal = (
    <Flex
      h="100%"
      direction="column"
      w="100%"
      css={{ borderLeft: "1px solid #666" }}
    >
      <Flex
        fontSize="12px"
        h="25px"
        bg="#1E1E1E"
        color="#999"
        css={{ border: "1px solid #666" }}
      >
        {map(v => {
          return (
            <Flex
              flex={1}
              align="center"
              css={{
                borderRight: "1px solid #666",
                cursor: "pointer",
                _hover: { opacity: 0.75 },
              }}
              px={2}
              bg={ttab === v.key ? "#5137C5" : ""}
              color={ttab === v.key ? "#ddd" : "#999"}
              justify="center"
              onClick={async () => {
                if (ttab === v.key && v.key === "lua" && global.prompt) {
                  setDryrun(!dryrun)
                  const on = !global.dryrun
                  global.setDryrun(on)
                  await global.prompt(
                    "toggling dryrun mode...... " + (on ? "on" : "off")
                  )
                } else {
                  setTtab(v.key)
                }
              }}
            >
              {v.name}
            </Flex>
          )
        })(ttabs)}
      </Flex>
      <Box flex={1} w="100%" h="100%" bg="#1E1E1E" position="relative">
        {ttab !== "log" ? null : (
          <Box
            p={2}
            bg="red"
            fontSize="11px"
            position="abosolute"
            h="100%"
            w="100%"
            bg="#1e1e1e"
            color="#c6c6c6"
            fontFamily="monospace"
            css={{ overflowY: "auto" }}
          >
            {map(v => {
              return (
                <Box>
                  <Box as="span" color="#666" mr={2}>
                    [{`${dayjs(v.date).format("MM/DD HH:mm:ss")}`}]
                  </Box>{" "}
                  {v.desc}
                </Box>
              )
            })(logs)}
          </Box>
        )}
        <Box id="terminal" borderRadius="0" w="100%" h="100%">
          <XTerminal {...{ global }} />
        </Box>
      </Box>
    </Flex>
  )

  return (
    <Box
      ref={containerRef}
      flex={1}
      h="100%"
      w="100%"
      css={{ overflow: "hidden" }}
    >
      {terminal}
    </Box>
  )
}
