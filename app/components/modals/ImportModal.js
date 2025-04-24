import { Input, NativeSelect, Box, Flex } from "@chakra-ui/react"
import Modal from "/components/modals/Modal"
import use from "/lib/use"
import { useState } from "react"
import g from "/lib/global"
import { generateId, filterFiles } from "/lib/utils"
import { src_data_js, src_data_lua } from "/lib/scripts"
import { append } from "ramda"
import lf from "localforage"

export default function ImportModal() {
  const [modal7, setModal7] = use("modal7")
  const [files, setFiles] = use("files")
  const [file, setFile] = use("file")
  const [openFiles, setOpenFiles] = use("openFiles")
  const [preview, setPreview] = use("preview")
  const [selDir, setSelDir] = use("selDir")
  const handleImportClick = () => {
    g.fileInputRef.current.click()
  }
  const handleFileChange = event => {
    const file = event.target.files[0]
    if (file) {
      const reader = new FileReader()
      reader.onload = async e => {
        const txt = e.target.result
        const id = generateId()
        const fileext = file.name.split(".").pop().toLowerCase()
        let name = file.name
        for (let f of files) {
          if (
            f.pid === selDir.pid &&
            f.name === name &&
            f.path === selDir.path
          ) {
            name = `${id.split("-")[0]}.${fileext}`
          }
        }
        const _file = {
          name,
          update: Date.now(),
          id,
          ext: fileext,
          pid: selDir.pid,
          path: selDir.path,
        }
        const _files = append(_file, files)
        await lf.setItem("files", filterFiles(_files))
        await lf.setItem(`file-${id}`, txt)
        setFiles(_files)
        setOpenFiles([...openFiles, _file])
        setFile(_file)
        setPreview(fileext === "md")
        g.setType(fileext)
        event.target.value = ""
        g.setType(fileext)
        // todo: handle this better
        setTimeout(() => g.editorRef.current.setValue(txt), 100)
        setModal7(false)
      }

      reader.readAsText(file)
    }
  }

  return (
    <Modal {...{ modal: modal7, setModal: setModal7 }}>
      <Box p={6}>
        <Box color="#5137C5" fontWeight="bold" mb={4} fontSize="20px">
          Import File
        </Box>
        <Box fontSize="12px" color="#666" mb={2} mt={4}>
          Imoprt Location
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
          onClick={handleImportClick}
        >
          <input
            type="file"
            ref={g.fileInputRef}
            onChange={handleFileChange}
            accept=".lua, .js, .json, .md, .ts"
            style={{ display: "none" }}
          />
          Choose File
        </Flex>
      </Box>
    </Modal>
  )
}
