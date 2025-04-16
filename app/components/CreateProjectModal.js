import { Input, Box, Flex } from "@chakra-ui/react"
import lf from "localforage"
import Modal from "/components/Modal"
import use from "/lib/use"
import { useState } from "react"
import { generateId, DateMS, filterProjects } from "/lib/utils"
import { append } from "ramda"

export default function CreateProjectModal() {
  const [projects, setProjects] = use("projects")
  const [modal3, setModal3] = use("modal3")
  const [projectname, setProjectname] = useState("")
  return (
    <Modal {...{ modal: modal3, setModal: setModal3 }}>
      <Box p={6}>
        <Box color="#5137C5" fontWeight="bold" mb={4} fontSize="20px">
          Create New Project
        </Box>
        <Box fontSize="12px" color="#666" mb={2}>
          Project Name
        </Box>
        <Flex align="flex-end">
          <Input
            flex={1}
            value={projectname}
            onChange={e => setProjectname(e.target.value)}
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
            if (/^\s*$/.test(projectname)) return alert("Enter a project name.")
            const id = generateId()
            const _pr = {
              name: projectname,
              created: DateMS.now(),
              id,
              open: true,
            }
            const _prs = append(_pr, projects)
            await lf.setItem("projects", filterProjects(_prs))
            setProjects(_prs)
            setProjectname("")
            setModal3(false)
          }}
        >
          Create
        </Flex>
      </Box>
    </Modal>
  )
}
