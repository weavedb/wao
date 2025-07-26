import { structured_from, structured_to } from "../src/structured.js"
import { cases_from, cases_to } from "./lib/structured_cases.js"
import { normalize } from "../src/erl_json.js"
import { genTest } from "./lib/test-utils.js"
import { ok, httpsig_errors } from "./lib/cases.js"
import { httpsig_from, httpsig_to } from "../src/httpsig.js"

genTest({
  its: [
    {
      it: "should test httpsig_to",
      cases: cases_from,
      path: "/~hbsig@1.0/httpsig_to",
      pmod: v => structured_from(normalize(v)),
      mod: v => httpsig_to(normalize(v)),
    },
    {
      it: "should test httpsig_to",
      cases: ok,
      path: "/~hbsig@1.0/httpsig_to",
      pmod: v => structured_from(normalize(v)),
      mod: v => httpsig_to(normalize(v)),
    },
  ],
})
