import use from "/lib/use"
import _assert from "assert"
import { Box, Flex } from "@chakra-ui/react"
import g from "/lib/global"
import { prepend, map } from "ramda"
import { generateId, dayjs, filterFiles } from "/lib/utils"
import lf from "localforage"

export default function Left() {
  const [tests, setTests] = use("tests")
  const [test, setTest] = use("test")
  const [files, setFiles] = use("files")
  const [file, setFile] = use("file")
  const [proc, setProc] = use("proc")
  const [tab, setTab] = use("tab")
  const buttons = (
    <Flex
      h="60px"
      align="center"
      p={4}
      fontSize="12px"
      css={{ borderBottom: "1px solid #ddd" }}
    >
      {!file || file.ext !== "js" ? null : (
        <>
          <Flex
            py={1}
            px={3}
            fontSize="10px"
            color="#ddd"
            bg="#5137C5"
            css={{
              borderRadius: "5px",
              cursor: "pointer",
              _hover: { opacity: 0.75 },
            }}
            onClick={async () => {
              try {
                const js = g.editorRef.current.getValue()
                const p = proc ? g.ao.p(proc.id) : null
                let descs = []
                const src = async path => {
                  for (let v of files) {
                    if (v.name === path) {
                      return await lf.getItem(`file-${v.id}`)
                    }
                  }
                  return null
                }
                const assert = _assert
                const require = async name => {
                  let _module = { exports: null }
                  const js = await src(name)
                  eval(js)
                  return _module.exports
                }
                let i = 0
                const it = (desc, fn) => {
                  descs[i].tests.push({ desc, fn })
                }

                const describe = (desc2, fn) => {
                  descs.push({ desc: desc2, fn, tests: [] })
                }
                eval(js)
                const ts = Date.now()
                let success = 0
                let fail = 0
                let res = []
                for (let v of descs) {
                  let _res = []
                  let _success = 0
                  let _fail = 0
                  await v.fn({ require })
                  for (let v2 of v.tests) {
                    const start = Date.now()
                    try {
                      await v2.fn({
                        ao: g.ao,
                        src,
                        p,
                      })
                      _res.push({
                        description: v2.desc,
                        success: true,
                        error: null,
                        duration: Date.now() - start,
                      })
                      _success++
                      success++
                    } catch (e) {
                      _res.push({
                        description: v2.desc,
                        success: false,
                        error: e.toString(),
                        duration: Date.now() - start,
                      })
                      _fail++
                      fail++
                    }
                  }
                  res.push({
                    description: v.desc,
                    cases: _res,
                    success: _success,
                    fail: _fail,
                  })
                  i++
                }
                const result = {
                  file: file.name,
                  process: proc?.id ?? null,
                  id: generateId(),
                  date: ts,
                  duration: Date.now() - ts,
                  tests: res,
                  success,
                  fail,
                }
                if (success > 0 || fail > 0) {
                  setTab("Tests")
                  setTest(result)
                  setTests([result, ...tests])
                }
              } catch (e) {
                console.log(e)
              }
            }}
          >
            Run Test
          </Flex>
          <Box flex={1} />
        </>
      )}
    </Flex>
  )
  return (
    <Box
      fontSize="12px"
      w="315px"
      h="100%"
      css={{ borderRight: "1px solid #ddd", overflowY: "auto" }}
    >
      {buttons}
      {map(v => (
        <Flex
          h="50px"
          bg={v.id === test?.id ? "#5137C5" : "white"}
          fontSize="12px"
          p={4}
          direction="column"
          justify="center"
          onClick={async () => {
            setTest(v)
          }}
          css={{
            borderBottom: "1px solid #ddd",
            cursor: "pointer",
            _hover: { opacity: 0.75 },
          }}
        >
          <Flex
            align="center"
            fontWeight="bold"
            color={v.id !== test?.id ? "#5137C5" : "#ddd"}
          >
            <Box
              mr={4}
              px={2}
              bg="#bbb"
              color="#222"
              fontWeight="normal"
              css={{ borderRadius: "3px" }}
            >
              {v.file}
            </Box>
            success {v.success} : fail {v.fail} : {v.duration} ms
          </Flex>
          <Box color={v.id !== test?.id ? "#222" : "#ddd"}>
            {dayjs(v.date).fromNow()}
          </Box>
        </Flex>
      ))(tests)}
    </Box>
  )
}
