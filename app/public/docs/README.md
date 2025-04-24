# WAO - Wizard AO SDK & Testing

WAO SDK streamlines Arweave/AO development with elegant syntax enhancements and seamless message piping for enjoyable coding experiences. GraphQL operations are also made super easy.

Additionally, it includes a drop-in replacement for `aoconnect`, allowing the testing of lua scripts 1000x faster than the mainnet by emulating AO units in memory. It's even 100x faster than testing with [arlocal](https://github.com/textury/arlocal) and [ao-localnet](https://github.com/permaweb/ao-localnet).

- [Tutorials](./tutorials/README.md)
- [API Reference](./api/README.md)


# Quick Start

With browser-embedded AO units, you can spin up a localnet, spawn processes, send messages, write tests, and debug—all in your browser.

For the most productive development workflow, write tests locally and hook them into the browser for debugging.

#### 1. Create an APP

```bash
npx wao create waoapp && cd waoapp
```

#### 2. Run WAO Proxy

```bash
npx wao proxy
```

#### 3. Connect the Browser to the Proxy

Go to [the web app](https://preview.wao.eco/) and open `Networks`, then click `Proxy`.

#### 4. Run Test

```bash
yarn test test/test.js
```

#### 5. Deploy a Process to AO Testnet

```bash
yarn deploy src/counter.lua --wallet PATH_TO_WALLET_JSON
```

---

<nav style="display:flex;justify-content:space-between;">
  <a></a>
  <a href="./tutorials/README.md">Tutorials</a>
</nav>
