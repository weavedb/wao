import { Input, NativeSelect, Box, Flex } from "@chakra-ui/react"
import Modal from "/components/modals/Modal"
import use from "/lib/use"
import { useState } from "react"
import g from "/lib/global"
import { generateId, filterFiles } from "/lib/utils"
import { hb_url, src_data_js, src_data_lua } from "/lib/scripts"
import { append, indexBy, prop } from "ramda"
import lf from "localforage"
import { Adaptor } from "wao/web"
import { DataItem } from "arbundles"
import Hub from "/lib/hub"
import { tags } from "/lib/utils"
import { toaster } from "@/components/ui/toaster"
import md5 from "md5"

export default function FSModal() {
  const [modal6, setModal6] = use("modal6")
  const [port, setPort] = useState("4006")
  const [wsid, setWSID] = use("wsid")
  const [localFS, setLocalFS] = use("localFS")
  const [fsPort, setFSPort] = use("fsPort")

  return (
    <Modal {...{ modal: modal6, setModal: setModal6 }}>
      <Box p={6}>
        <Box color="#5137C5" fontWeight="bold" mb={4} fontSize="20px">
          Connect with Local FS
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
            const getP = v => {
              if (v.p.length === 0) return "/" + v.name
              else return "/" + v.p.join("/") + "/" + v.name
            }
            const dirmap = indexBy(prop("id"), localFS)
            const updateDir = _localFS => {
              const _files = []
              const ls = (fs, p = [], dpath = []) => {
                for (let k in fs) {
                  const path = `/${p.length === 0 ? "" : p.join("/") + "/"}`
                  const _dpath = `/${dpath.length === 0 ? "" : dpath.join("/") + "/"}`
                  const id = md5(
                    `2${_dpath}${k}`.replace(new RegExp("/", "g"), "#")
                  )
                  _files.push({
                    dir: typeof fs[k] === "object",
                    open: dirmap[id]?.open ?? false,
                    name: k,
                    pid: "2",
                    p,
                    ext: typeof fs[k] === "object" ? null : k.split(".").pop(),
                    id,
                    path,
                    local: true,
                    filename: `${_dpath}${k}`,
                  })
                  if (typeof fs[k] === "object")
                    ls(fs[k], [...p, id], [...dpath, k])
                }
              }
              ls(_localFS)
              setLocalFS(_files)
            }
            g.hub1 = new Hub(`ws://localhost:${port}`)
            g.hub1.onMsg = async obj => {
              console.log("New FS Msg:", obj)
              if (obj.subtype === "content") {
                const ext = obj.path.split(".").pop()
                const file = g.fileRef.current
                const id = md5(
                  `2${obj.path}`.replace(new RegExp("/", "g"), "#")
                )
                await lf.setItem(`file-${id}`, obj.content)
                if (file?.id === id) {
                  setTimeout(() => {
                    g.setType(ext)
                    g.editorRef.current.setValue(obj.content)
                  }, 100)
                }
              } else if (obj.subtype === "dir_change") updateDir(obj.dir)
            }

            g.hub1.onClose = () => {
              setWSID(null)
              setFSPort(null)
              toaster.create({
                type: "error",
                description: "Local FS disconnected!",
              })
            }

            g.hub1.onRegister = msg => {
              console.log("are we here??")
              setWSID(msg.id)
              let lfs = null
              setFSPort(port)
              updateDir(msg.dir)
              setModal6(false)
            }

            g.hub1.onError = () => {
              toaster.create({
                type: "error",
                description: "Failed to connect",
              })
            }

            g.hub1.connect()
          }}
        >
          Connect
        </Flex>
      </Box>
    </Modal>
  )
}
