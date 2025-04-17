import { Input, NativeSelect, Box, Flex } from "@chakra-ui/react"
import Modal from "/components/modals/Modal"
import use from "/lib/use"
import { useState } from "react"
import g from "/lib/global"
import { generateId, DateMS, filterFiles } from "/lib/utils"
import { src_data_js, src_data_lua } from "/lib/scripts"
import { append } from "ramda"
import lf from "localforage"

export default function CreateFileModal() {
  const [modal, setModal] = use("modal")
  const [filename, setFilename] = useState("")
  const [fileext, setFileext] = useState("js")
  const [files, setFiles] = use("files")
  const [file, setFile] = use("file")
  const [openFiles, setOpenFiles] = use("openFiles")
  const [preview, setPreview] = use("preview")
  const [selDir, setSelDir] = use("selDir")
  return (
    <Modal {...{ modal, setModal }}>
      <Box p={6}>
        <Box color="#5137C5" fontWeight="bold" mb={4} fontSize="20px">
          Create New File
        </Box>
        <Box fontSize="12px" color="#666" mb={2}>
          File Name
        </Box>
        <Flex align="flex-end">
          <Input
            flex={1}
            value={filename}
            onChange={e => setFilename(e.target.value)}
          />
          <Box mx={2}>.</Box>
          <NativeSelect.Root w="80px">
            <NativeSelect.Field
              value={fileext}
              onChange={e => setFileext(e.target.value)}
            >
              <option value="lua">lua</option>
              <option value="js">js</option>
              <option value="json">json</option>
              <option value="md">md</option>
            </NativeSelect.Field>
            <NativeSelect.Indicator />
          </NativeSelect.Root>
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
            const id = generateId()
            console.log(id)
            const name = `${filename}.${fileext}`
            for (let f of files) {
              if (
                f.pid === selDir.pid &&
                f.name === name &&
                f.path === selDir.path
              ) {
                alert("File already exists!")
                return
              }
            }
            const _file = {
              name,
              update: DateMS.now(),
              id,
              ext: fileext,
              pid: selDir.pid,
              path: selDir.path,
            }
            const txt =
              fileext === "js"
                ? src_data_js
                : fileext === "lua"
                  ? src_data_lua
                  : ""
            const _files = append(_file, files)
            await lf.setItem("files", filterFiles(_files))
            await lf.setItem(`file-${id}`, txt)
            setFiles(_files)
            setOpenFiles([...openFiles, _file])
            setFile(_file)
            setPreview(false)
            g.setType(fileext)
            // todo: handle this better
            setTimeout(() => g.editorRef.current.setValue(txt), 100)
            setModal(false)
            setFilename("")
          }}
        >
          Create
        </Flex>
      </Box>
    </Modal>
  )
}
