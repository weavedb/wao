---
name: create-aos
description: "Scaffold a new AOS Lua script with its test file. Use when user says 'create a handler', 'new AOS script', 'scaffold a script', or 'add a handler'. Not for building full scripts with iteration — use /build-aos."
argument-hint: "<script-name>"
disable-model-invocation: true
---

Scaffold a new AOS script with its test file.

The argument `$ARGUMENTS` is the script name (required). It should be lowercase and will be used as the filename.

## Steps

0. Check for conflicts:
```bash
test -f "src/$ARGUMENTS.lua" && echo "WARNING: src/$ARGUMENTS.lua already exists" || echo "OK"
```
If the file exists, ask the user: overwrite or choose a different name?

1. Create the Lua script file at `src/$ARGUMENTS.lua`:

```lua
local State = State or {}

Handlers.add("{Name}", "{Name}", function(msg)
  msg.reply({ Data = "{Name} handled!" })
end)
```

Where `{Name}` is the argument with the first letter capitalized (e.g., `greeting` -> `Greeting`).

2. Create the test file at `test/$ARGUMENTS.test.js`:

```js
import assert from "assert"
import { describe, it } from "node:test"
import { readFileSync } from "fs"
import { resolve } from "path"

import { AO, acc } from "wao/test"

const src_data = readFileSync(
  resolve(import.meta.dirname, "../src/{name}.lua"),
  "utf8"
)

describe("{Name}", function () {
  it("should handle {Name} action", async () => {
    const ao = await new AO().init(acc[0])
    const { p } = await ao.deploy({ src_data })
    const result = await p.d("{Name}", false)
    assert.equal(result, "{Name} handled!")
  })
})
```

3. Run the new test:

```bash
yarn test test/{name}.test.js
```

4. Report whether the test passes. If it fails, fix and re-run.
