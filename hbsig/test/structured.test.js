import { structured_from, structured_to } from "../src/structured.js"
import { cases_from, cases_to } from "./lib/structured_cases.js"
import { ok } from "./lib/cases.js"
import { normalize } from "../src/erl_json.js"
import { genTest } from "./lib/test-utils.js"

genTest({
  its: [
    {
      it: "should test structured_from (cases_from)",
      path: "/~hbsig@1.0/structured_from",
      cases: cases_from,
      // For simple string-only cases, the output should match the input
      // HB's structured codec doesn't add ao-types for pure string values
      mod: v => normalize(v),
      skipAoTypes: true,
      removeAoTypes: true,
    },
    {
      it: "should test structured_from (ok cases)",
      path: "/~hbsig@1.0/structured_from",
      cases: ok,
      // For complex cases, use structured_from to get expected TABM format
      mod: v => structured_from(normalize(v)),
      skipAoTypes: true,
      removeAoTypes: true,
    },
    {
      it: "should test structured_to (cases_to)",
      path: "/~hbsig@1.0/structured_to",
      cases: cases_to,
      mod: v => structured_to(normalize(v)),
    },
  ],
})
