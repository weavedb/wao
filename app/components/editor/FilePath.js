import { Box, Flex, Icon } from "@chakra-ui/react"
import { FaAngleRight } from "react-icons/fa6"
import { indexBy, prop } from "ramda"
import use from "/lib/use"

export default function FilePath({}) {
  const [file] = use("file")
  const [localFS, setLocalFS] = use("localFS")
  const [localFSOpen, setLocalFSOpen] = use("localFSOpen")
  const [projects] = use("projects")
  if (!file) return null
  const _projects = [
    ...(localFS.length > 0
      ? [{ id: "2", name: "Local Computer", open: localFSOpen }]
      : []),
    ...projects,
  ]

  const pmap = indexBy(prop("id"))(_projects)
  let html = []
  html.push(<Box>{pmap[file.pid].name}</Box>)
  for (let v of file.path.split("/")) {
    if (v === "") continue
    html.push(
      <>
        <Icon boxSize="11px" mx={2}>
          <FaAngleRight />
        </Icon>
        <Box>{v}</Box>
      </>
    )
  }
  return (
    <>
      {html}
      <Icon boxSize="11px" mx={2}>
        <FaAngleRight />
      </Icon>
      <Box>{file?.name}</Box>
    </>
  )
}
