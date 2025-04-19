// react
import { PanelGroup, Panel, PanelResizeHandle } from "react-resizable-panels"
import use from "/lib/use"

// chakra-ui
import { Box, Flex, Icon } from "@chakra-ui/react"
import { Toaster } from "@/components/ui/toaster"

// utils
import { includes } from "ramda"

// components
import GlobalStyle from "/components/styles/GlobalStyle"
import Global from "/components/Global"
import Header from "/components/Header"
import Left from "/components/Left"
import Middle from "/components/Middle"
import Sidebar from "/components/Sidebar"
import Logo from "/components/Logo"
import Footer from "/components/Footer"
import Editor from "/components/editor/Editor"
import Terminal from "/components/terminal/Terminal"
import CreateFileModal from "/components/modals/CreateFileModal"
import CreateFolderModal from "/components/modals/CreateFolderModal"
import LaunchNetworkModal from "/components/modals/LaunchNetworkModal"
import CreateProjectModal from "/components/modals/CreateProjectModal"

export default function Home({}) {
  const [tab, setTab] = use("tab")
  const [init, setInit] = use("init")
  return (
    <>
      <GlobalStyle />
      <Global />
      {!init ? <Logo /> : null}
      <Flex direction="column" h="100vh" w="100vw">
        <Flex h="30px">
          <Header />
        </Flex>
        <Flex h="calc(100vh - 60px)" w="100vw" css={{ overflow: "hidden" }}>
          <Flex w="60px" h="calc(100vh - 60px)">
            <Sidebar />
          </Flex>
          {includes(tab, [
            "Modules",
            "Processes",
            "Messages",
            "Entity",
          ]) ? null : (
            <Flex w="315px" h="calc(100vh - 60px)">
              <Left />
            </Flex>
          )}
          <Box
            flex={1}
            h="calc(100vh - 60px)"
            w="100%"
            css={{ overflow: "hidden" }}
          >
            <PanelGroup direction="horizontal">
              {!includes(tab, [
                "Projects",
                "Modules",
                "Processes",
                "Messages",
                "Entity",
              ]) ? (
                <>
                  <Panel defaultSize={35} minSize={20} order={1}>
                    <Box h="calc(100vh - 60px)">
                      <Middle />
                    </Box>
                  </Panel>
                  <PanelResizeHandle />
                </>
              ) : tab !== "Projects" ? (
                <>
                  <Panel defaultSize={50} minSize={20} order={1}>
                    <Box h="calc(100vh - 60px)">
                      <Middle />
                    </Box>
                  </Panel>
                  <PanelResizeHandle />
                </>
              ) : null}
              <Panel minSize={30} defaultSize={35} order={2}>
                <PanelGroup
                  direction={tab !== "Projects" ? "vertical" : "horizontal"}
                >
                  <Panel maxSize={65}>
                    <Editor />
                  </Panel>
                  <PanelResizeHandle />
                  <Panel maxSize={65}>
                    <Terminal />
                  </Panel>
                </PanelGroup>
              </Panel>
            </PanelGroup>
          </Box>
        </Flex>
        <Footer />
      </Flex>
      <CreateFileModal />
      <LaunchNetworkModal />
      <CreateProjectModal />
      <CreateFolderModal />
      <Toaster />
    </>
  )
}
