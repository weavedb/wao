import { flat_from, flat_to } from "../src/flat.js"
import { cases_from, cases_to } from "./lib/flat_cases.js"
import { normalize } from "../src/erl_json.js"
import { genTest } from "./lib/test-utils.js"

genTest({
  its: [
    {
      it: "should test flat_from",
      cases: cases_from,
      path: "/~hbsig@1.0/flat_from",
      mod: v => flat_from(normalize(v)),
    },
    {
      it: "should test flat_to",
      cases: cases_to,
      path: "/~hbsig@1.0/flat_to",
      mod: v => flat_to(normalize(v)),
    },
  ],
})
