import { Flex } from "@chakra-ui/react"
import use from "/lib/use"

import MiddleTests from "/components/middle/MiddleTests"
import MiddleModules from "/components/middle/MiddleModules"
import MiddleProcesses from "/components/middle/MiddleProcesses"
import MiddleMessages from "/components/middle/MiddleMessages"
import MiddleNetworks from "/components/middle/MiddleNetworks"

export default function Middle() {
  const [tab, setTab] = use("tab")
  const [init, setInit] = use("init")
  if (!init) return null
  const middle = (() => {
    switch (tab) {
      case "Projects":
        return null
      case "Tests":
        return <MiddleTests />
      case "Modules":
        return <MiddleModules />
      case "Processes":
        return <MiddleProcesses />
      case "Messages":
        return <MiddleMessages />
      case "Networks":
        return <MiddleNetworks />
    }
  })()
  return <Flex w="100%">{!init ? null : <Flex w="100%">{middle}</Flex>}</Flex>
}
