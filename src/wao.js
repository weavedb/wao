import BAO from "./bao.js"
import { connect } from "./aoconnect-web.js"
import AR from "./war.js"
import * as WarpArBundles from "warp-arbundles"
const pkg = WarpArBundles.default ?? WarpArBundles
const { createData, ArweaveSigner } = pkg

function createDataItemSigner(wallet) {
  const signer = async ({ data, tags, target, anchor }) => {
    const signer = new ArweaveSigner(wallet)
    const dataItem = createData(data, signer, { tags, target, anchor })
    const sig = dataItem.sign(signer).then(async () => {
      return {
        id: await dataItem.id,
        raw: await dataItem.getRaw(),
      }
    })
    return sig
  }
  return signer
}

class AO extends BAO {
  constructor(opt = {}) {
    super({ ...opt, connect, AR, createDataItemSigner })
  }
}

export default AO
