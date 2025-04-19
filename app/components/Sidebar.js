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
  FaWallet,
  FaCode,
  FaBug,
  FaCodeCompare,
  FaEnvelopesBulk,
  FaCubes,
  FaNetworkWired,
} from "react-icons/fa6"

const tabmap = {
  Projects: { icon: <FaCode /> },
  Modules: { icon: <FaCubes /> },
  Processes: { icon: <FaCodeCompare /> },
  Messages: { icon: <FaEnvelopesBulk /> },
  //Accounts: { icon: <FaWallet /> },
  //    Tokens: { icon: <FaCoins /> },
  //    Storage: { icon: <FaHardDrive /> },
  //    Database: { icon: <FaDatabase /> },
  Networks: { icon: <FaNetworkWired /> },
  Tests: { icon: <FaBug /> },
}
const tabs = keys(tabmap)

export default function Sidebar({}) {
  const [tab, setTab] = use("tab")
  const [init] = use("init")
  const [proc] = use("proc")
  const [acc] = use("acc")
  return !init ? null : (
    <Flex css={{ overflowY: "auto", borderRight: "1px solid #ddd" }}>
      <Flex direction="column" w="60px">
        {map(v => {
          return (
            <Tooltip
              content={v.toUpperCase()}
              positioning={{ placement: "right-end" }}
              openDelay={0}
              closeDelay={0}
            >
              <Flex
                h="60px"
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
                  if (v === "Accounts" && acc === null) return
                  if (includes(v, ["Tokens", "Storage", "Database"])) {
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
