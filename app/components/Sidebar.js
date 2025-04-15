import {
  Input,
  NativeSelect,
  Image,
  Box,
  Flex,
  Textarea,
  Icon,
} from "@chakra-ui/react"
import { Tooltip } from "@/components/ui/tooltip"
import { keys, map, includes } from "ramda"
import use from "/lib/use"

import {
  FaCode,
  FaBug,
  FaCodeCompare,
  FaEnvelopesBulk,
  FaCubes,
  FaNetworkWired,
} from "react-icons/fa6"

const tabmap = {
  Projects: { icon: <FaCode /> },
  Tests: { icon: <FaBug /> },
  Modules: { icon: <FaCubes /> },
  Processes: { icon: <FaCodeCompare /> },
  Messages: { icon: <FaEnvelopesBulk /> },
  //    Accounts: { icon: <FaWallet /> },
  //    Tokens: { icon: <FaCoins /> },
  //    Storage: { icon: <FaHardDrive /> },
  //    Database: { icon: <FaDatabase /> },
  Networks: { icon: <FaNetworkWired /> },
}
const tabs = keys(tabmap)

export default function Sidebar({}) {
  const [tab, setTab] = use("tab")
  const [init] = use("init")
  const [proc] = use("proc")
  return !init ? null : (
    <Flex css={{ overflowY: "auto", borderRight: "1px solid #ddd" }}>
      <Flex direction="column" w="50px">
        {map(v => {
          return (
            <Tooltip
              content={v.toUpperCase()}
              positioning={{ placement: "right-end" }}
              openDelay={0}
              closeDelay={0}
            >
              <Flex
                h="50px"
                w="100%"
                fontSize="12px"
                align="center"
                css={{
                  cursor: "pointer",
                  _hover: { opacity: 0.75 },
                  flexShrink: "0",
                }}
                onClick={() => {
                  if (v === "Messages" && proc === null) return
                  if (
                    includes(v, ["Accounts", "Tokens", "Storage", "Database"])
                  ) {
                    return alert("Coming Soon!")
                  }
                  setTab(v)
                }}
                bg={v === tab ? "#5137C5" : ""}
                color={
                  includes(v, ["Accounts", "Tokens", "Storage", "Database"]) ||
                  (v === "Messages" && proc === null)
                    ? "#999"
                    : v === tab
                      ? "white"
                      : ""
                }
                justify="center"
              >
                <Icon size="lg">{tabmap[v].icon}</Icon>
              </Flex>
            </Tooltip>
          )
        })(tabs)}
      </Flex>
    </Flex>
  )
}
