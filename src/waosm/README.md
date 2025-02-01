# Waosm Compressor

Waosm is a lightweight new compression algorithm to efficiently compress Wasm memory.

It is implemented in Rust and compiled to Wasm for better performance in browsers.

```bash
yarn build
```

Using Waosm in nodejs.

```js
import init, { Compressor, Decompressor } from "../src/waosm-node.js"

const main = async ()=>{
  await init()
  const compressor = new Compressor()
  const decompressor = new Decompressor()
  const memory = new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8, 9, 0])
  console.log(memory)
  const compressed = compressor.compress(memory)
  console.log(compressed)
  const decompressed = decompressor.decompress(compressed)
  console.log(decompressed)
}
main()
```

## How Compression Works

Waosm Compression is a simple LRF variant that sequentially processes 8-bit integers. It is specifically optimized for zero-padded Wasm memory, achieving a 99% compression rate for largely idle WebAssembly memory. This enables efficient handling of large WebAssembly modules in browser environments.

The algorithm packs integers into chunks with 3 parts.

- flag
- 256-base number to tell the number of element in the current chunk
- actuall 8-bit intergers

### Flag

A flag is a 1 byte (8 bits) integer, which ranges from 0 to 255. So there are 3 digits.

- 1st digit: `mode`
  - `0`: a sequence of 0s
  - `1`: a sequence of a single repeating 8 bit integer
  - `2`: a sequence of multiple unique 8 bit integer
- 2nd digit:
  - `0`: the 3rd digit is the number of elements, no 256-base number required in this case
  - `1-9`: the number of integers to represent the 256-base number
- 3rd digit:
    - if the 2nd digit is `0`, the 3rd digit represents the number of the elements, `0` represents `10`
	- otherwise, the 3rd digit represents the first integer in the 256-base number representation
	
### 256-Base Number

This part tells how many 8-bit integers are in the current chunk. For instance,
- `2, 200` = (256 ** 1 * 2) + 200 = 712
- `5, 50, 200` = (256 ** 2 * 5) + (256 ** 1 * 50) + 200 = 340680

Note that for space efficiency, the first integer is in the flag in some cases.

### 8 Bit Elements

- `Mode 0`: no need for this part since we know it's a sequence of 0
- `Mode 1`: 1 integer to tell which integer of a sequence this chunk is
- `Mode 2`: the entire integers are required

## Examples

`[ 1, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]` (28 bytes)

- `204` | `-` | `1, 2, 3, 4`
- `103` | `-` | `5`
- `204` | `-` | `6, 7, 8, 9`
- `010` | `17` | `-`

It becomes `[204, 1, 2, 3, 4, 103, 5, 204, 6, 7, 8, 9, 10, 17]` (14 bytes - 50% compression rate).

