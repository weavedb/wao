# Installing HyperBEAM and WAO

## Installing HyperBEAM

Follow [the HyperBEAM docs](https://hyperbeam.ar.io/run/running-a-hyperbeam-node.html) and install HyperBEAM on your local machine. You could use one of the existing remote nodes, but you'll miss many important details in these tutorials since we'll literally crack open the internals.

## Installing WAO

Create a WAO project that comes with the `wao` SDK and testing framework:

```bash
npx wao create myapp && cd myapp
```

You can also create an empty directory and install `wao`:

```bash
mkdir myapp && cd myapp && yarn init && yarn add wao
mkdir test && touch test/hyperbeam.js
```

Edit `package.json` to enable ESM and test commands with the `--experimental-wasm-memory64` flag:

```json
{
  "name": "myapp",
  "version": "0.0.1",
  "type": "module",
  "scripts": {
    "test": "node --experimental-wasm-memory64",
    "test-only": "node --experimental-wasm-memory64 --test-only",
  },
  "dependencies": {
    "wao": "^0.29.2"
  }
}
```

## Running Tests

Import the `HyperBEAM` and `HB` classes from `wao` to interact with your HyperBEAM node.

Make sure you have an Arweave wallet JWK at `HyperBEAM/.wallet.json` for the node operator account.

Here's the minimum viable test code. The `HyperBEAM` class starts up a HyperBEAM node and kills it once your tests complete, creating a sandbox environment for each test suite.

You can interact with any HyperBEAM node from JS using the `HB` class.

With these two classes, you can write complete test suites for your HyperBEAM node, devices, processes, and modules running on top (such as AOS) using only JavaScript.

```js
import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM, toAddr } from "wao/test"
import { HB } from "wao"
import { resolve } from "path"
import { readFileSync } from "fs"

/*
  The link to your HyperBEAM node directory.
  It's relative to your app root folder, not the test folder.
*/
const cwd = "../../HyperBEAM"

// operator wallet location
const wallet = resolve(process.cwd(), cwd, ".wallet.json")

const jwk = JSON.parse(readFileSync(wallet, "utf8")) // operator
const addr = toAddr(jwk.n) // operator address

describe("HyperBEAM", function () {
  let hbeam, hb 
  before(async () => {
    // start a hyperbeam node and wait till it's ready
    hbeam = await new HyperBEAM({ cwd }).ready()
  })

  beforeEach(async () => {
    // instantiate HB SDK with the operator wallet
	hb = await new HB({}).init(jwk)
  })

  // kill the node after testing
  after(async () => hbeam.kill())

  it("should run a HyperBEAM node", async () => {
    // get build info
    const { out } = await hb.get({ path: "/~meta@1.0/build" })
    assert.equal(out.version, toAddr(jwk.n))
  })
})
```

If you can't run HyperBEAM on your local machine, skip the `HyperBEAM` class and pass the remote node `url` to `HB`:

```js
import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { toAddr } from "wao/test"
import { HB } from "wao"
import { resolve } from "path"
import { readFileSync } from "fs"

// operator wallet location
const wallet = resolve(process.cwd(), cwd, ".wallet.json")

const jwk = JSON.parse(readFileSync(wallet, "utf8")) // operator
const addr = toAddr(jwk.n) // operator address

describe("HyperBEAM", function () {
  let hb 
  
  beforeEach(async () => {
    // instantiate HB SDK with the operator wallet
	hb = await new HB({ url: "http://localhost:10001" }).init(jwk)
  })

  it("should run a HyperBEAM node", async () => {
    // get build info
    const { out } = await hb.get({ path: "/~meta@1.0/build" })
    assert.equal(out.version, toAddr(jwk.n))
  })
})
```

Run tests:

```bash
yarn test test/hyperbeam.js
```

Now we're ready to decode HyperBEAM.
