import { Input, NativeSelect, Box, Flex } from "@chakra-ui/react"
import Modal from "/components/modals/Modal"
import use from "/lib/use"
import { useEffect, useState } from "react"
import g from "/lib/global"
import { generateId, filterFiles } from "/lib/utils"
import { src_data_js, src_data_lua } from "/lib/scripts"
import { append, assoc, clone } from "ramda"
import lf from "localforage"

export default function RenameFileModal() {
  const [modal8, setModal8] = use("modal8")
  const [file, setFile] = use("file")
  const [files, setFiles] = use("files")
  const [openFiles, setOpenFiles] = use("openFiles")
  const [filename, setFilename] = useState(
    file.name.split(".").slice(0, -1).join(".")
  )

  useEffect(() => {
    setFilename(file.name.split(".").slice(0, -1).join("."))
  }, [file])

  return (
    <Modal {...{ modal: modal8, setModal: setModal8 }}>
      <Box p={6}>
        <Box color="#5137C5" fontWeight="bold" mb={4} fontSize="20px">
          Rename File
        </Box>
        <Box fontSize="12px" color="#666" mb={2}>
          New File Name
        </Box>
        <Flex align="flex-end">
          <Input
            flex={1}
            value={filename}
            onChange={e => setFilename(e.target.value)}
          />
          <Box mx={2}>.</Box>
          {file.ext}
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
            if (/^\s*$/.test(filename)) return alert("Enter a filename.")
            const name = `${filename}.${file.ext}`
            for (let f of files) {
              if (
                f.pid === file.pid &&
                f.name === name &&
                f.path === file.path
              ) {
                alert("File already exists!")
                return
              }
            }
            const _file = assoc("name", name, file)
            let _files = clone(files)
            let i = 0
            for (let f of _files) {
              if (f.id === file.id) _files[i] = _file
              i++
            }
            let _openFiles = clone(openFiles)

            let i2 = 0
            for (let f of _openFiles) {
              if (f.id === file.id) _openFiles[i2] = _file
              i2++
            }
            await lf.setItem("files", filterFiles(_files))
            setFiles(_files)
            setOpenFiles(_openFiles)
            setFile(_file)
            setModal8(false)
            setFilename("")
          }}
        >
          Rename
        </Flex>
      </Box>
    </Modal>
  )
}
