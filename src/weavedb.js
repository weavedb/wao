import { resolve } from "path"
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "fs"
const KB = 1024
const MB = KB * 1024
const CACHE_SZ = 32 * KB
const CHUNK_SZ = 128 * MB
const NOTIFY_SZ = 512 * MB
const log = console.log
const rand = Math.floor(Math.random() * 1000000).toString()
import {
  encode,
  Encoder,
  decode,
  Decoder,
  Parser,
} from "../../arjson/sdk/src/index.js"
function tobits(arr, cursor = 0) {
  let bitStr = ""
  for (let i = 0; i < arr.length; i++) {
    bitStr += arr[i].toString(2).padStart(8, "0")
  }
  let remaining = bitStr.slice(cursor)

  let result = []
  let offset = cursor % 8
  if (offset !== 0) {
    let firstChunkSize = 8 - offset
    result.push(remaining.slice(0, firstChunkSize))
    remaining = remaining.slice(firstChunkSize)
  }
  while (remaining.length >= 8) {
    result.push(remaining.slice(0, 8))
    remaining = remaining.slice(8)
  }
  if (remaining.length > 0) result.push(remaining)
  return result
}

function frombits(bitArray) {
  // Join all bit strings
  const bitStr = bitArray.join("")

  // Calculate how many bytes we need
  const byteCount = Math.ceil(bitStr.length / 8)

  // Create a new Uint8Array
  const result = new Uint8Array(byteCount)

  // Fill the Uint8Array
  for (let i = 0; i < byteCount; i++) {
    // Get the next 8 bits (or fewer for the last byte)
    const start = i * 8
    const end = Math.min(start + 8, bitStr.length)
    const bits = bitStr.substring(start, end).padEnd(8, "0")

    // Convert the bits to a byte
    result[i] = parseInt(bits, 2)
  }

  return result
}

export default class WeaveDB {
  constructor(ar, dir) {
    dir ??= resolve(import.meta.dirname, ".db")
    const kv_dir = resolve(dir, rand)
    const data_dir = resolve(kv_dir, "data")
    const rollup_dir = resolve(kv_dir, "rollup")
    for (const v of [dir, kv_dir, rollup_dir, data_dir]) {
      if (!existsSync(v)) mkdirSync(v)
    }
    this.ext = (mod, FS) => {
      let cache = { data: {}, rollup: {} }
      return {
        async create(id) {
          let properties = { isDevice: false, contents: null }
          if (!FS.analyzePath("/rollup/").exists) FS.mkdir("/rollup/")
          let node = FS.createFile("/", "rollup/" + id, properties, true, false)

          // Set initial parame
          let data = await ar.data(id, null, log)
          const bytesLength = data?.length ?? 0
          node.total_size = Number(bytesLength)
          node.cache = new Uint8Array(0)
          node.position = 0

          // Add a function that defers querying the file size until it is asked the first time.
          Object.defineProperties(node, {
            usedBytes: { get: () => bytesLength },
          })

          // Now we have created the file in the emscripten FS, we can open it as a stream
          let stream = FS.open("/rollup/" + id, "r")

          //console.log("JS: Created file: ", id, " fd: ", stream.fd);
          return stream
        },
        async createData(col, doc, val) {
          let properties = { isDevice: false, contents: null }
          if (!FS.analyzePath("/data/").exists) FS.mkdir("/data/")
          if (!FS.analyzePath(`/data/${col}`).exists) FS.mkdir(`/data/${col}`)
          let node = FS.createFile(
            "/",
            `data/${col}/${doc}`,
            properties,
            true,
            false
          )

          // Set initial parame
          let data = val
          if (!val) {
            const col_dir = resolve(data_dir, col)
            const _data =
              readFileSync(resolve(col_dir, `${doc}.json`), "utf8") ?? ""
            data = Buffer.from(_data, "utf8")
          }
          const bytesLength = data?.length ?? 0
          node.total_size = Number(bytesLength)
          node.cache = new Uint8Array(0)
          node.position = 0

          // Add a function that defers querying the file size until it is asked the first time.
          Object.defineProperties(node, {
            usedBytes: {
              get: function () {
                return this.total_size
              },
            },
          })

          // Now we have created the file in the emscripten FS, we can open it as a stream
          let stream = FS.open("/data/" + `${col}/${doc}`, "r")

          //console.log("JS: Created file: ", id, " fd: ", stream.fd);
          return stream
        },
        async open(filename, val) {
          const pathCategory = filename.split("/")[1]
          if (pathCategory === "rollup") {
            //log("JS: Opening ID: ", id)
            const id = filename.split("/")[2]
            if (FS.analyzePath(filename).exists) {
              let stream = FS.open(filename, "r")
              if (stream.fd) return stream.fd
              console.log("JS: File not found: ", filename)
              return 0
            } else {
              //console.log("JS: Open => Creating file: ", id);
              const stream = await this.create(id)
              //console.log("JS: Open => Created file: ", id, " fd: ", stream.fd);
              return stream.fd
            }
          } else if (pathCategory === "data") {
            //log("JS: Opening ID: ", id)
            const col = filename.split("/")[2]
            const doc = filename.split("/")[3]
            if (FS.analyzePath(filename).exists) {
              let stream = FS.open(filename, "r")
              if (stream.fd) return stream.fd
              console.log("JS: File not found: ", filename)
              return 0
            } else {
              //console.log("JS: Open => Creating file: ", id);
              const stream = await this.createData(col, doc, val)
              //console.log("JS: Open => Created file: ", id, " fd: ", stream.fd);
              return stream.fd
            }
          } else {
            console.log("JS: Invalid path category: ", pathCategory)
            return 0
          }
        },
        async read(fd, raw_dst_ptr, raw_length, val) {
          let to_read = Number(raw_length)
          let dst_ptr = Number(raw_dst_ptr)
          let stream = 0
          for (let i = 0; i < FS.streams.length; i++) {
            if (FS.streams[i].fd === fd) stream = FS.streams[i]
          }
          // Satisfy what we can with the cache first
          let bytes_read = this.readFromCache(stream, dst_ptr, to_read)
          stream.node.position += bytes_read
          stream.lastReadPosition = stream.node.position
          dst_ptr += bytes_read
          to_read -= bytes_read

          // Return if we have satisfied the request

          //console.log("KV: Satisfied request with cache. Returning...")
          if (to_read === 0) return bytes_read

          //console.log("KV: Read from cache: ", bytes_read, " Remaining to read: ", to_read)

          const chunk_download_sz = Math.max(to_read, CACHE_SZ)
          const to = Math.min(
            stream.node.total_size,
            stream.node.position + chunk_download_sz
          )
          let data = val
          if (!data) {
            const sp = stream.path.split("/")
            if (sp[1] === "data") {
              const col_dir = resolve(data_dir, sp[2])
              const _data =
                readFileSync(resolve(col_dir, `${sp[3]}.json`), "utf8") ?? ""
              data = Buffer.from(_data, "utf8")
            } else {
              data = await ar.data(stream.node.name, null, log)
              let updates = []
              const isNext = b => b.length > 1 || (b.length === 1 && b[0] !== 0)
              try {
                while (isNext(data)) {
                  let d = new Decoder()
                  let { op, col, doc, json, left, len } = decode(data, d, true)
                  data = frombits(left)
                  if (op === 2) {
                    const col_dir = resolve(data_dir, col.toString())
                    const _data = JSON.parse(
                      readFileSync(resolve(col_dir, `${doc}.json`), "utf8")
                    )
                    let u = new Encoder()
                    const e = encode(_data, u)
                    let p = new Parser(d.cols())
                    const res = p.update(e, data, len)
                    json = res.json
                    data = res.left
                  } else if (op === 3) json = null
                  updates.push({ col: col, doc: doc, json })
                }

                for (const v of updates) {
                  const val = Buffer.from(JSON.stringify(v.json))
                  const _fd = await this.open(`/data/${v.col}/${v.doc}`, val)
                  const col_dir = resolve(data_dir, v.col.toString())
                  if (!existsSync(col_dir)) mkdirSync(col_dir)
                  writeFileSync(
                    resolve(col_dir, `${v.doc}.json`),
                    JSON.stringify(v.json)
                  )

                  for (let i = 0; i < FS.streams.length; i++) {
                    if (FS.streams[i].fd === _fd) {
                      FS.streams[i].node.total_size = val.length
                      break
                    }
                  }
                  await this.read(_fd, _fd, val.length, val)
                }
              } catch (e) {
                log(e)
              }
            }
          }
          // Extract the Range header to determine the start and end of the requested chunk
          const start = 0
          const end = data.length
          // Create a ReadableStream for the requested chunk
          const chunk = data.subarray(start, end)
          const response = new Response(
            new ReadableStream({
              start(controller) {
                controller.enqueue(chunk) // Push the chunk to the stream
                controller.close() // Close the stream when done
              },
            }),
            {
              headers: { "Content-Length": chunk.length.toString() },
            }
          )
          const reader = response.body.getReader()
          let bytes_until_cache = CHUNK_SZ
          let bytes_until_notify = NOTIFY_SZ
          let downloaded_bytes = 0
          let cache_chunks = []

          try {
            while (true) {
              const { done, value: chunk_bytes } = await reader.read()
              if (done) break
              // Update the number of downloaded bytes to be _all_, not just the write length
              downloaded_bytes += chunk_bytes.length
              bytes_until_cache -= chunk_bytes.length
              bytes_until_notify -= chunk_bytes.length

              // Write bytes from the chunk and update the pointer if necessary
              const write_length = Math.min(chunk_bytes.length, to_read)
              if (write_length > 0) {
                //console.log("KV: Writing: ", write_length, " bytes to: ", dst_ptr)
                mod.HEAP8.set(chunk_bytes.subarray(0, write_length), dst_ptr)
                dst_ptr += write_length
                bytes_read += write_length
                stream.node.position += write_length
                to_read -= write_length
              }

              if (to_read == 0) {
                // Add excess bytes to our cache
                const chunk_to_cache = chunk_bytes.subarray(write_length)
                //console.log("KV: Cacheing excess: ", chunk_to_cache.length)
                cache_chunks.push(chunk_to_cache)
              }
              if (bytes_until_cache <= 0) {
                console.log("KV: Chunk size reached. Compressing cache...")
                stream.node.cache = this.addChunksToCache(
                  stream.node.cache,
                  cache_chunks
                )
                cache_chunks = []
                bytes_until_cache = CHUNK_SZ
              }

              if (bytes_until_notify <= 0) {
                console.log(
                  "KV: Downloaded: ",
                  (downloaded_bytes / stream.node.total_size) * 100,
                  "%"
                )
                bytes_until_notify = NOTIFY_SZ
              }
            }
          } catch (error) {
            console.error("KV: Error reading the stream: ", error)
          } finally {
            reader.releaseLock()
          }
          // If we have no cache, or we have not satisfied the full request, we need to download the rest
          // Rebuild the cache from the new cache chunks
          stream.node.cache = this.addChunksToCache(
            stream.node.cache,
            cache_chunks
          )

          // Update the last read position
          stream.lastReadPosition = stream.node.position
          return bytes_read
        },
        close(fd) {
          let stream = 0
          for (let i = 0; i < FS.streams.length; i++) {
            if (FS.streams[i].fd === fd) stream = FS.streams[i]
          }
          FS.close(stream)
        },

        readFromCache(stream, dst_ptr, length) {
          // Check if the cache has been invalidated by a seek
          if (stream.lastReadPosition !== stream.node.position) {
            //console.log("KV: Invalidating cache for fd: ", stream.fd, " Current pos: ", stream.node.position, " Last read pos: ", stream.lastReadPosition)
            stream.node.cache = new Uint8Array(0)
            return 0
          }
          // Calculate the bytes of the request that can be satisfied with the cache
          let cache_part_length = Math.min(length, stream.node.cache.length)
          let cache_part = stream.node.cache.subarray(0, cache_part_length)
          mod.HEAP8.set(cache_part, dst_ptr)
          // Set the new cache to the remainder of the unused cache and update pointers
          stream.node.cache = stream.node.cache.subarray(cache_part_length)

          return cache_part_length
        },

        addChunksToCache(old_cache, chunks) {
          // Make a new cache array of the old cache length + the sum of the chunk lengths, capped by the max cache size
          let new_cache_length = Math.min(
            old_cache.length +
              chunks.reduce((acc, chunk) => acc + chunk.length, 0),
            CACHE_SZ
          )
          let new_cache = new Uint8Array(new_cache_length)
          // Copy the old cache to the new cache
          new_cache.set(old_cache, 0)
          // Load the cache chunks into the new cache
          let current_offset = old_cache.length
          for (let chunk of chunks) {
            if (current_offset < new_cache_length) {
              new_cache.set(
                chunk.subarray(0, new_cache_length - current_offset),
                current_offset
              )
              current_offset += chunk.length
            }
          }
          return new_cache
        },
      }
    }
  }
}
