import { Input, Box, Flex } from "@chakra-ui/react"
import lf from "localforage"
import Modal from "/components/modals/Modal"
import use from "/lib/use"
import { useState } from "react"
import { append } from "ramda"

export default function LaunchNetworkModal() {
  const [modal2, setModal2] = use("modal2")
  const [cache, setCache] = use("cache")
  const [ctype, setCtype] = use("ctype")
  const [networks, setNetworks] = use("networks")
  const [ntag, setNtag] = useState("")
  const [nver, setNver] = useState("")
  const [ndesc, setNdesc] = useState("")

  return (
    <Modal {...{ modal: modal2, setModal: setModal2 }}>
      <Box p={6}>
        <Box color="#5137C5" fontWeight="bold" mb={4} fontSize="20px">
          Launch New AO Network
        </Box>

        <Box>
          <Box fontSize="12px" color="#666" mb={2}>
            Network Tag
          </Box>
          <Flex align="flex-end">
            <Input value="ao" disabled={true} w="100px" />
            <Box mx={2}>.</Box>
            <Input
              w="100px"
              placeholder="WLN"
              value={ntag}
              onChange={e => setNtag(e.target.value)}
            />
            <Box mx={2}>.</Box>
            <Input
              w="100px"
              placeholder="1"
              value={nver}
              onChange={e => setNver(e.target.value)}
            />
          </Flex>
        </Box>
        <Box flex={1} mt={4}>
          <Box fontSize="12px" color="#666" mb={2}>
            Description
          </Box>
          <Input
            placeholder=""
            value={ndesc}
            onChange={e => setNdesc(e.target.value)}
          />
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
            if (/^\s*$/.test(ntag.trim())) return alert("Enter a network name")
            if (/^\s*$/.test(nver.trim()))
              return alert("Enter a network version")
            const tag = `ao.${ntag}.${nver}`
            for (let v of networks) {
              if (tag === v.tag) return alert(`${ntag} already exists!`)
            }

            const _networks = append({ tag, desc: ndesc }, networks)
            setNetworks(_networks)
            setCache(tag)
            setCtype(tag)
            setNtag("")
            setNver("")
            setNdesc("")
            setModal2(false)
            await lf.setItem("networks", _networks)
          }}
        >
          Launch
        </Flex>
      </Box>
    </Modal>
  )
}
