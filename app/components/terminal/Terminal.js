import { Box, Flex } from "@chakra-ui/react"
import { map } from "ramda"
import dynamic from "next/dynamic"
import { dayjs } from "/lib/utils"
import g from "/lib/global"
import use from "/lib/use"

const XTerminal = dynamic(
  () => import("/components/terminal/Xterm").then(mod => mod),
  { ssr: false }
)

export default function Terminal({}) {
  const [ttab, setTtab] = use("ttab")
  const [dryrun, setDryrun] = use("dryrun")
  const [logs, setLogs] = use("logs")
  const ttabs = [
    {
      key: "lua",
      name: `AOS ${dryrun ? " ( DRYRUN )" : ""}`,
    },
    { key: "log", name: "LOGS" },
  ]

  const terminal = (
    <Flex
      h="100%"
      direction="column"
      w="100%"
      css={{ borderLeft: "1px solid #666" }}
    >
      <Flex
        fontSize="12px"
        h="30px"
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
                if (ttab === v.key && v.key === "lua" && g.prompt) {
                  setDryrun(!dryrun)
                  const on = !g.dryrun
                  g.setDryrun(on)
                  await g.prompt(
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
          <XTerminal {...{ global: g }} />
        </Box>
      </Box>
    </Flex>
  )

  return (
    <Box
      ref={g.containerRef}
      flex={1}
      h="100%"
      w="100%"
      css={{ overflow: "hidden" }}
    >
      {terminal}
    </Box>
  )
}
