import use from "/lib/use"

import LeftProjects from "/components/left/LeftProjects"
import LeftTests from "/components/left/LeftTests"
import LeftModules from "/components/left/LeftModules"
import LeftProcesses from "/components/left/LeftProcesses"
import LeftMessages from "/components/left/LeftMessages"
import LeftNetworks from "/components/left/LeftNetworks"

export default function Left() {
  const [tab, setTab] = use("tab")
  const [init, setInit] = use("init")
  if (!init) return null
  switch (tab) {
    case "Projects":
      return <LeftProjects />
    case "Tests":
      return <LeftTests />
    case "Modules":
      return <LeftModules />
    case "Processes":
      return <LeftProcesses />
    case "Messages":
      return <LeftMessages />
    case "Networks":
      return <LeftNetworks />
  }
}
