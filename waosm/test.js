import { Compressor, Decompressor } from "../src/waosm-node.js"

const main = async () => {
  const compressor = new Compressor()
  const decompressor = new Decompressor()
  const memory = new Uint8Array([
    1, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0,
  ])
  console.log(memory)
  const compressed = compressor.compress(memory)
  console.log(compressed)
  const decompressed = decompressor.decompress(compressed)
  console.log(decompressed)
}
main()
