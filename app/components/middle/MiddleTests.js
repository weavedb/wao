import { Icon, Box, Flex } from "@chakra-ui/react"
import { addIndex, includes, map } from "ramda"
import { dayjs } from "/lib/utils"
import g from "/lib/global"
import use from "/lib/use"
import { FaX, FaCheck } from "react-icons/fa6"

export default function Middle() {
  const [test, setTest] = use("test")
  const [init, setInit] = use("init")
  const [tab, setTab] = use("tab")
  return (
    <Flex w="100%">
      {!test ? null : (
        <Box
          px={4}
          py={2}
          fontSize="11px"
          flex={1}
          h="calc(100vh - 110px)"
          css={{ overflowY: "auto" }}
        >
          <Flex my={2} fontWeight="bold" color="#5137C5">
            Stats
          </Flex>
          {map(v => {
            if (includes(v.name, ["signature", "signature-input"])) {
              return null
            }
            return (
              <Flex my={2} align="center">
                <Box
                  w="130px"
                  color="white"
                  bg="#5137C5"
                  px={2}
                  mr={4}
                  css={{ borderRadius: "3px" }}
                >
                  {v.name}
                </Box>
                <Box
                  flex={1}
                  css={{ wordBreak: "break-all", whiteSpace: "wrap" }}
                >
                  {v.value}
                </Box>
              </Flex>
            )
          })([
            { name: "Main Process", value: test.process },
            { name: "Test File", value: test.file },
            { name: "Duration", value: `${test.duration} ms` },
            { name: "Success", value: `${test.success}` },
            { name: "Fail", value: `${test.fail}` },
            { name: "Date", value: dayjs(test.date).fromNow() },
          ])}
          <Flex mt={4} fontWeight="bold" color="#5137C5">
            Results
          </Flex>

          {addIndex(map)((v, i) => {
            return (
              <>
                <Flex
                  mt={4}
                  fontSize="12px"
                  p={1}
                  mb={2}
                  bg={v.fail === 0 ? "#4e9a06" : "#cc0000"}
                  color="#ddd"
                  css={{ borderRadius: "3px" }}
                >
                  <Flex px={2} align="center" w="100%">
                    <Box mr={3}>{i + 1}</Box>
                    <Box>{v.description}</Box>
                    <Box flex={1} />
                    <Flex mr={4}>
                      <Icon size="sm" mr={2}>
                        <FaCheck />
                      </Icon>
                      {v.success}
                    </Flex>
                    <Flex>
                      <Icon size="sm" mr={2}>
                        <FaX />
                      </Icon>
                      {v.fail}
                    </Flex>
                  </Flex>
                </Flex>
                {addIndex(map)((v2, i2) => {
                  return (
                    <Flex
                      fontSize="11px"
                      ml={10}
                      p={1}
                      mb={2}
                      color="#eee"
                      css={{
                        borderRadius: "3px",
                      }}
                      bg={v2.success ? "#4e9a06" : "#cc0000"}
                      direction="column"
                    >
                      <Flex>
                        <Flex px={2} align="center" w="100%">
                          <Box mr={3}>
                            <Icon size="sm">
                              {v2.success ? <FaCheck /> : <FaX />}
                            </Icon>
                          </Box>
                          <Box>{v2.description}</Box>
                          <Box flex={1} />
                          <Box>{v2.duration} ms</Box>
                        </Flex>
                      </Flex>

                      {v2.error ? (
                        <Flex mt={1} pl={9} fontSize="10px">
                          {v2.error}
                        </Flex>
                      ) : null}
                    </Flex>
                  )
                })(v.cases)}
              </>
            )
          })(test.tests)}
        </Box>
      )}
    </Flex>
  )
}
