### Setting up a Project

It's super easy to set up a test AO project manually.

```bash
mkdir wao-test && cd wao-test
yarn init && yarn add wao
```

Add `test` and `test-only` commands to your `package.json`.

```json
{
  "scripts": {
    "test": "node --experimental-wasm-memory64",
    "test-only": "node --experimental-wasm-memory64 --test-only"
  }
}
```

Create `test` directory and `test.js` file.

```bash
mkdir test && touch test/test.js
```

---

<nav style="display:flex;justify-content:space-between;">
  <a href="./aoconnect-wrapper.md">Aoconnect Wrapper</a>
  <a href="./write-tests.md">Writing Tests</a>
</nav>

