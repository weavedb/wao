import { Input, Box, Flex } from "@chakra-ui/react"
import lf from "localforage"
import Modal from "/components/modals/Modal"
import use from "/lib/use"
import { useState } from "react"
import { generateId, filterFiles } from "/lib/utils"
import { append } from "ramda"
import g from "/lib/global"

export default function CreateProjectModal() {
  const [modal4, setModal4] = use("modal4")
  const [dirname, setDirname] = useState("")
  const [selDir, setSelDir] = use("selDir")
  const [files, setFiles] = use("files")
  return (
    <Modal {...{ modal: modal4, setModal: setModal4 }}>
      {" "}
      <Box p={6}>
        <Box color="#5137C5" fontWeight="bold" mb={4} fontSize="20px">
          Create New Folder
        </Box>
        <Box fontSize="12px" color="#666" mb={2}>
          Folder Name
        </Box>
        <Flex align="flex-end">
          <Input
            flex={1}
            value={dirname}
            onChange={e => setDirname(e.target.value)}
          />
        </Flex>
        <Box fontSize="12px" color="#666" mb={2} mt={4}>
          Location
        </Box>
        <Box fontSize="12px" mb={6}>
          {g.getDirs()}
        </Box>
        <Flex
          align="center"
          justify="center"
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
            if (/^\s*$/.test(dirname)) return alert("Enter a folder name.")
            const id = generateId()
            for (let f of files) {
              if (
                f.pid === selDir.pid &&
                f.name === dirname &&
                f.path === selDir.path
              ) {
                alert("Directory already exists!")
                return
              }
            }
            const _file = {
              name: `${dirname}`,
              update: Date.now(),
              id,
              pid: selDir.pid,
              dir: true,
              path: selDir.path,
            }
            const _files = append(_file, files)
            await lf.setItem("files", filterFiles(_files))
            setFiles(_files)
            setModal4(false)
            setDirname("")
          }}
        >
          Create
        </Flex>
      </Box>
    </Modal>
  )
}
