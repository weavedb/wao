import { structured_from, structured_to } from "../src/structured.js"
import { cases_from, cases_to } from "./lib/structured_cases.js"
import { ok } from "./lib/cases.js"
import { normalize } from "../src/erl_json.js"
import { genTest } from "./lib/test-utils.js"

genTest({
  its: [
    {
      it: "should test structured_from",
      path: "/~hbsig@1.0/structured_from",
      cases: cases_from,
      mod: v => structured_from(normalize(v)),
    },
    {
      it: "should test structured_from",
      path: "/~hbsig@1.0/structured_from",
      cases: ok,
      mod: v => structured_from(normalize(v)),
    },
  ],
})
