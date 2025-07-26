import { cases_from } from "./lib/erl_json_cases.js"
import all_cases from "./lib/all_cases.js"
import { gen } from "./lib/gen.js"
import { genTest } from "./lib/test-utils.js"

genTest({
  its: [{ cases: cases_from }, { cases: all_cases }, { cases: gen(100) }],
})
