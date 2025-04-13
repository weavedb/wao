### HB

`HB` handles interactions with a [Hyperbeam](https://permaweb.github.io/HyperBEAM/) node.

- [Instantiate](#instantiate-5)
- [metrics](#metrics)
- [info](#info)
- [messages](#messages)
- [process](#process)
- [schedule](#schedule)
- [compute](#compute)
- [dryrun](#dryrun)
- [get](#get)
- [post](#post)
- [request](#request)

#### Instantiate

```js
import { HB } = "wao"
const hb = await new HB({ url: "http://localhost:10000" }).init(jwk)
```

#### metrics

Get node metrics.

```js
const metrics = await hb.metrics()
```

#### info

Get node info.

```js
const info = await hb.info()
const operator = info.address
```
#### messages

Get messages on a process. You can get next messages by `message.next`. 

```js
const msgs = await hb.messages({ target, from, to })
for(const item of msgs.edges){
  const { cursor: slot, node: { assignment, message } } = item
}
if(msgs.next) const msgs2 = await msg.next()
```

- `target` : a process id
- `from` | `to` : the slot numbers. Messages are marked by integer on HB, not by txid. `0` is the `Type=Process` message that spawed the process. `to` is inclusive. Both are optional. 

#### process

Equivalent to spawning a process.

```js
const pid = await hb.process({ tags, data })
```

#### schedule

Equivalent to sending a message.

```js
const slot = await hb.schedule({ tags, data, process: pid, action })
```

#### compute

Equivalent to getting a result.

```js
const res = await hb.compute({ process: pid, slot })
const { Messages, Spawns, Assignments, Output } = res
```
#### dryrun

```js
const res = await hb.dryrun({ tags, data, process: pid, action })
const { Messages, Spawns, Assignments, Output } = res
```

#### get

Sending a signed http request with `GET` method to HyperBEAM.

```js
const res = await hb.get({ device, path })
```

#### post

Sending a signed http request with `POST` method to HyperBEAM.

```js
const res = await hb.post({ device, path, tags })
```

#### request

Sending a signed http request to HyperBEAM.

```js
const res = await hb.request({ device, path, tags, method })
```

---

<nav style="display:flex;justify-content:space-between;">
  <a href="./armem.md">ArMem</a>
  <a></a>
</nav>
