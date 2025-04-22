#### 1. Create an APP

```bash
npx wao create waoapp && cd waoapp
```

#### 2. Run WAO Proxy

```bash
npx wao proxy
```

#### 3. Connect the Browser to the Proxy

Go to [the web app](https://wao-localnet.vercel.app/) and open `Networks`, then click `Proxy`.

#### 4. Run Test

```bash
yarn test test/test.js
```

#### 5. Deploy a Process to AO Testnet

```bash
yarn deploy src/counter.lua --wallet PATH_TO_WALLET_JSON
```
