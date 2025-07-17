# Installing HyperBEAM and WAO

## Installing HyperBEAM

Follow [the HyperBEAM docs](https://hyperbeam.ar.io/run/running-a-hyperbeam-node.html) and install HyperBEAM on your local machine. You could use one of the existing remote nodes, but you'll miss many important details in these tutorials since we'll literally crack open the internals.

## Installing WAO

Create a WAO project that comes with the `wao` SDK and testing framework:

```bash [Terminal]
npx wao create myapp && cd myapp
```

You can also create an empty directory and install `wao`:

```bash [Terminal]
mkdir myapp && cd myapp && yarn init && yarn add wao
mkdir test && touch test/hyperbeam.js
```

Edit `package.json` to enable ESM and test commands with the `--experimental-wasm-memory64` flag and disable concurrency so the test won't try running multiple HyperBEAM nodes:

```json [/package.json]
{
  "name": "myapp",
  "version": "0.0.1",
  "type": "module",
  "scripts": {
    "test": "node --experimental-wasm-memory64 --test --test-concurrency=1",
    "test-only": "node --experimental-wasm-memory64 --test-only --test-concurrency=1",
    "test-all": "node --experimental-wasm-memory64 --test --test-concurrency=1 test/**/*.test.js"
  },
  "dependencies": {
    "wao": "^0.29.2"
  }
}
```

## Writing Tests

Import the `HyperBEAM` and `HB` classes from `wao` to interact with your HyperBEAM node.

Make sure you have an Arweave wallet JWK at `HyperBEAM/.wallet.json` for the node operator account.

Here's the minimum viable test code. The `HyperBEAM` class starts up a HyperBEAM node and kills it once your tests complete, creating a sandbox environment for each test suite.


```js [/test/hyperbeam.test.js]
import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { HyperBEAM } from "wao/test"

/*
  The link to your HyperBEAM node directory.
  It's relative to your app root folder, not the test folder.
*/
const cwd = "../HyperBEAM"

describe("HyperBEAM", function () {
  let hbeam, hb

  // start a hyperbeam node and wait till it's ready, reset node storage
  before(async () => {
    hbeam = await new HyperBEAM({ cwd, reset: true }).ready()
  }) 

  // HB class from hbeam has the node operator signer
  beforeEach(async () => (hb = hbeam.hb))

  // kill the node after testing
  after(async () => hbeam.kill())

  it("should run a HyperBEAM node", async () => {
    // change config
    await hb.post({ path: "/~meta@1.0/info", test_config: "abc" })

    // get config
    const { out } = await hb.get({ path: "/~meta@1.0/info" })
    assert.equal(out.test_config, "abc")
  })
})
```

You can interact with any HyperBEAM node from JS using the `HB` class.

With these two classes, you can write complete test suites for your HyperBEAM node, devices, processes, and modules running on top (such as AOS) using only JavaScript.

If you can't run HyperBEAM on your local machine, skip the `HyperBEAM` class and pass the remote node `url` to `HB`:

```js [/test/hb.test.js]
import assert from "assert"
import { describe, it, before, after, beforeEach } from "node:test"
import { acc } from "wao/test"
import { HB } from "wao"

const cwd = "../HyperBEAM"

describe("HyperBEAM", function () {
  let hb

  // using one of the pre-generated non-operator accounts for test
  beforeEach(async () => {
    hb = new HB({ jwk: acc[0].jwk, url: "http://localhost:10001" })
  })

  it("should connect to a HyperBEAM node", async () => {
    // get build info
    const build = await hb.g("/~meta@1.0/build")
    assert.equal(build.node, "HyperBEAM")
  })
})
```

## Running Tests

You can find the working test files for this chapter here:

- [hyperbeam.test.js](https://github.com/weavedb/wao/blob/master/dhfs-tutorial-app/test/hyperbeam.test.js)
- [hb.test.js](https://github.com/weavedb/wao/blob/master/dhfs-tutorial-app/test/hyperbeam.test.js)

Run tests:

```bash [Terminal]
yarn test test/hyperbeam.test.js
# yarn test test/hb.testjs
```

Now we're ready to decode HyperBEAM.

## References

##### General

- [HyperBEAM Installation Guide](https://hyperbeam.ar.io/run/running-a-hyperbeam-node.html)

##### WAO API 
- [HyperBEAM Class API](/api/hyperbeam)
- [HB Class API](/api/hb)
