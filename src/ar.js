import _Arweave from "arweave"
const Arweave = _Arweave.default ?? _Arweave
import { ArweaveSigner, bundleAndSignData, createData } from "arbundles"
import { buildTags, tag, isLocalhost } from "./utils.js"
import { is, isNil } from "ramda"
import GQL from "./gql.js"

class AR {
  constructor({ host, port = 443, protocol } = {}) {
    this.__type__ = "ar"
    let _arweave = { host, port, protocol }
    if (!_arweave.host)
      _arweave.host = port === 443 ? "arweave.net" : "localhost"
    if (!_arweave.protocol)
      _arweave.protocol = isLocalhost(_arweave.host) ? "http" : "https"
    if (!_arweave.port) _arweave.port = isLocalhost(_arweave.host) ? 1984 : 443
    this.port = _arweave.port
    this.host = _arweave.host
    this.protocol = _arweave.protocol
    this.arweave = Arweave.init(_arweave)
    const ArweaveUtils = this.arweave.utils

    this.arweave.transactions.getData = async function (id, options) {
      let data = undefined
      try {
        data = await this.chunks.downloadChunkedData(id)
      } catch (error) {}
      if (!data) {
        try {
          const {
            data: resData,
            ok,
            status,
            statusText,
          } = await this.api.get(`/${id}`, { responseType: "arraybuffer" })
          if (!ok) {
            throw new Error(`Bad http status code`, {
              cause: { status, statusText },
            })
          }
          data = resData
        } catch (error) {
          console.error(
            `Error while trying to download contiguous data from gateway cache for ${id}`
          )
          console.error(error)
        }
      }
      if (!data) {
        throw new Error(`${id} data was not found!`)
      }
      if (options && options.decode && !options.string) return data

      if (options && options.decode && options.string) {
        return ArweaveUtils.bufferToString(data)
      }
      return ArweaveUtils.bufferTob64Url(data)
    }

    this.gql = new GQL({
      url: `${_arweave.protocol}://${_arweave.host}:${_arweave.port}/graphql`,
    })
  }

  isArConnect(jwk) {
    return this.jwk?.id || this.jwk?.walletName === "ArConnect"
  }

  async init(jwk) {
    let isGen = false
    if (!jwk && typeof window === "object") jwk = window.arweaveWallet
    if (!jwk) isGen = true
    else {
      jwk = typeof jwk?.jwk === "object" ? jwk.jwk : jwk
      this.jwk = jwk
      const isWallet = this.isArConnect(this.jwk)
      if (isWallet) {
        try {
          await this.jwk.connect([
            "ACCESS_ADDRESS",
            "ACCESS_PUBLIC_KEY",
            "SIGN_TRANSACTION",
          ])
          this.addr = await this.jwk.getActiveAddress()
          this.pub = await this.jwk.getActivePublicKey()
          this.isWallet = true
        } catch (e) {
          isGen = true
        }
      } else {
        this.addr = await this.toAddr(jwk)
        this.pub = jwk.n
      }
    }
    if (isGen) {
      this.isWallet = false
      this.addr = this.pub = this.jwk = null
      await this.gen("100")
    }
    return this
  }

  async mine() {
    await this.arweave.api.get(`/mine`)
  }

  async checkWallet({ jwk } = {}) {
    let start = Date.now()
    if (jwk) return { err: null, jwk }
    let [err, addr, pub] = [null, null, null]
    let existWallet = typeof window === "object" && window.arweaveWallet
    let isJwkWallet = this.isArConnect(this.jwk)
    if (!this.jwk) {
      if (this._jwk) {
        jwk = this._jwk
        pub = this._jwk.n
        addr = await this.toAddr(jwk)
      } else {
        ;({ jwk, addr, pub } = await this.gen("100", false))
      }
    } else if (this.jwk && !isJwkWallet) {
      jwk = this.jwk
      addr = this.addr
      pub = this.pub
    } else if (existWallet) {
      await arweaveWallet.connect([
        "ACCESS_ADDRESS",
        "ACCESS_PUBLIC_KEY",
        "SIGN_TRANSACTION",
      ])
      const _addr = await arweaveWallet.getActiveAddress()
      if (_addr) {
        if (this.addr && this.addr !== _addr) err = "the wrong wallet"
        else {
          addr = _addr
          pub = await arweaveWallet.getActivePublicKey()
          jwk = arweaveWallet
        }
      } else {
        err = "no wallet found"
      }
    } else {
      err = "no wallet found"
    }
    return { addr, jwk, pub, err }
  }

  async balance(addr = this.addr) {
    if (!addr) {
      ;({ addr } = await this.checkWallet())
    }
    return this.toAR(await this.arweave.wallets.getBalance(addr))
  }

  async mint(addr, amount = "1.0") {
    await this.arweave.api.get(`/mint/${addr}/${this.toWinston(amount)}`)
    await this.mine()
    return await this.balance(addr)
  }

  toWinston(ar) {
    return this.arweave.ar.arToWinston(ar)
  }

  toAR(w) {
    return this.arweave.ar.winstonToAr(w)
  }

  async toAddr(jwk) {
    ;({ jwk } = await this.checkWallet({ jwk }))
    if (this.isArConnect(jwk)) return await jwk.getActiveAddress()
    else {
      return await this.arweave.wallets.jwkToAddress(jwk)
    }
  }

  async gen(amount, overwrite) {
    const jwk = await this.arweave.wallets.generate()
    const addr = await this.toAddr(jwk)
    if (overwrite === false) {
      this._jwk = jwk
    } else if (!this.jwk || overwrite) {
      this.jwk = jwk
      this.pub = jwk.n
      this.addr = addr
      this.isWallet = false
    }
    let bal = "0"
    if (amount && isLocalhost(this.host)) bal = await this.mint(addr, amount)
    return { jwk, addr, pub: jwk.n, bal }
  }

  async transfer(ar, target, jwk) {
    let err = null
    ;({ jwk, err } = await this.checkWallet({ jwk }))
    if (err) return { err }
    else {
      let tx = await this.arweave.createTransaction({
        target,
        quantity: this.toWinston(ar),
      })
      return await this.postTx(tx, jwk)
    }
  }

  async bundle(_items, jwk) {
    let err = null
    ;({ jwk, err } = await this.checkWallet({ jwk }))
    if (err) return { err }
    else {
      const signer = new ArweaveSigner(jwk)
      const items = _items.map(v => {
        let tags = []
        for (const k in v[1] && {}) {
          if (is(Array)(v[1][k])) {
            for (const v of v[1][k]) tags.push(tag(k, v))
          } else {
            tags.push(tag(k, v[1][k]))
          }
        }
        return createData(v[0], signer, { tags, ...(v[2] ?? {}) })
      })
      const bundle = await bundleAndSignData(items, signer)
      const tx = await bundle.toTransaction({}, this.arweave, jwk)
      await this.postTx(tx, jwk)
      return { err, items, tx, id: tx.id }
    }
  }

  async post({ data = "1984", tags = {}, jwk }) {
    let err = null
    ;({ err, jwk } = await this.checkWallet({ jwk }))
    if (err) return { err }
    else {
      let tx = await this.arweave.createTransaction({ data: data })
      let _tags = buildTags(null, tags)
      for (const v of _tags) tx.addTag(v.name, v.value)
      return await this.postTx(tx, jwk)
    }
  }

  async postTx(tx, jwk) {
    let [res, err] = [null, null]
    ;({ err, jwk } = await this.checkWallet({ jwk }))
    if (!err) {
      if (this.isArConnect(jwk)) tx = await jwk.sign(tx)
      else {
        await this.arweave.transactions.sign(tx, jwk)
      }
      res = await this.arweave.transactions.post(tx)
      if (res.status !== 200) err = res.statusText
      if (isLocalhost(this.host)) await this.mine()
    }
    return { res, err, id: tx.id }
  }

  async tx(txid) {
    return (await this.gql.txs({ id: txid }))[0] ?? null
  }

  async data(txid, _string = false) {
    let decode = true
    let string = _string
    if (is(Object, _string)) {
      if (!isNil(_string.decode)) decode = _string.decode
      if (!isNil(_string.string)) string = _string.string
    }
    const _data = await this.arweave.transactions.getData(txid, {
      decode,
      string,
    })
    return _data
  }
}

export default AR
