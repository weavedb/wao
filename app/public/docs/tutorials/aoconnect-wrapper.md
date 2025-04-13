### Drop-in `aoconnect` Replacement for Tests

By replacing `aoconnect` with WAO connect, everything runs in memory with zero latency and your tests are executed 1000x faster. The APIs are identical. So, there's no need to change anything else in your code.

```js
//import { spawn, message, dryrun, assign, result } from "@permaweb/aoconnect"
import { connect, acc } from "wao/test"
const { spawn, message, dryrun, assign, result } = connect()
```

---

<nav style="display:flex;justify-content:space-between;">
  <a href="./installation.md">Installation</a>
  <a href="./setup-project.md">Setting up Project</a>
</nav>
