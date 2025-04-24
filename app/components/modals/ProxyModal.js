import { Input, NativeSelect, Box, Flex } from "@chakra-ui/react"
import Modal from "/components/modals/Modal"
import use from "/lib/use"
import { useState } from "react"
import g from "/lib/global"
import { generateId, filterFiles } from "/lib/utils"
import { hb_url, src_data_js, src_data_lua } from "/lib/scripts"
import { append } from "ramda"
import lf from "localforage"
import { Adaptor } from "wao/web"
import { DataItem } from "arbundles"
import Hub from "/lib/hub"
import { tags } from "/lib/utils"
import { toaster } from "@/components/ui/toaster"

export default function ProxyModal() {
  const [modal5, setModal5] = use("modal5")
  const [port, setPort] = useState("4005")
  const [fileext, setFileext] = useState("js")
  const [files, setFiles] = use("files")
  const [file, setFile] = use("file")
  const [openFiles, setOpenFiles] = use("openFiles")
  const [preview, setPreview] = use("preview")
  const [selDir, setSelDir] = use("selDir")
  const [psid, setPSID] = use("psid")
  const [proxyPort, setProxyPort] = use("proxyPort")

  return (
    <Modal {...{ modal: modal5, setModal: setModal5 }}>
      <Box p={6}>
        <Box color="#5137C5" fontWeight="bold" mb={4} fontSize="20px">
          Connect with WAO Proxy
        </Box>
        <Box fontSize="12px" color="#666" mb={2}>
          Port
        </Box>
        <Flex align="flex-end">
          <Input
            flex={1}
            value={port}
            onChange={e => {
              if (!Number.isNaN(e.target.value * 1)) setPort(e.target.value)
            }}
          />
        </Flex>
        <Flex
          align="center"
          justify="center"
          mt={4}
          p={1}
          mb={1}
          color="#5137C5"
          css={{
            borderRadius: "3px",
            border: "1px solid #5137C5",
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
          onClick={async () => {
            const adaptor = new Adaptor({ hb_url, aoconnect: g.ao.mem })
            g.hub1 = new Hub("ws://localhost:4005")
            g.hub1.onMsg = async obj => {
              console.log("New PX Msg:", obj)
              adaptor.get(obj.req, res => {
                if (obj.req.device === "mu" && obj.req.path === "/") {
                  try {
                    let body = obj.req.body
                    if (
                      obj.req.body.type === "Buffer" &&
                      Array.isArray(obj.req.body.data)
                    ) {
                      body = new Uint8Array(body.data)
                    }

                    const _tags = new DataItem(body).tags
                    if (tags(_tags).Type === "Process") {
                      g.logSpawn(res.json.id)
                    } else {
                      g.logMsg(res.json.id)
                      g.addMsg(res.json.id)
                    }
                  } catch (e) {
                    console.log(e)
                  }
                }
                g.hub1.socket.send(
                  JSON.stringify({
                    type: "msg",
                    id: obj.id,
                    res: res ?? { status: 404, error: "not found" },
                  })
                )
              })
            }

            g.hub1.onClose = () => {
              setPSID(null)
              setProxyPort(null)
              toaster.create({
                type: "error",
                description: "WAO Proxy disconnected!",
              })
            }
            g.hub1.onRegister = msg => {
              setPSID(msg.id)
              setProxyPort(port)
              toaster.create({
                type: "success",
                description: `Connected with WAO Proxy: ${port}`,
              })
              setModal5(false)
            }
            g.hub1.onError = () => {
              toaster.create({
                type: "error",
                description: "Failed to connect",
              })
            }

            try {
              g.hub1.connect()
            } catch (e) {
              console.log(e)
            }
          }}
        >
          Connect
        </Flex>
      </Box>
    </Modal>
  )
}
