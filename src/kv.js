import Arweave from "arweave"
import { toGraphObj } from "./utils.js"
import { map } from "ramda"

const KB = 1024
const MB = KB * 1024
const CACHE_SZ = 32 * KB
const CHUNK_SZ = 128 * MB
const NOTIFY_SZ = 512 * MB
const log = console.log

export default class KV {
  constructor(ar) {
    this.kv = function WeaveDrive(mod, FS) {
      return {
        async create(id) {
          let properties = { isDevice: false, contents: null }
          if (!FS.analyzePath("/data/").exists) FS.mkdir("/data/")
          let node = FS.createFile("/", "data/" + id, properties, true, false)
          // Set initial parameters
          const bytesLength = (await ar.data(id))?.length ?? 0
          node.total_size = Number(bytesLength)
          node.cache = new Uint8Array(0)
          node.position = 0

          // Add a function that defers querying the file size until it is asked the first time.
          Object.defineProperties(node, {
            usedBytes: {
              get: () => bytesLength,
            },
          })

          // Now we have created the file in the emscripten FS, we can open it as a stream
          let stream = FS.open("/data/" + id, "r")

          //console.log("JS: Created file: ", id, " fd: ", stream.fd);
          return stream
        },
        async open(filename) {
          const pathCategory = filename.split("/")[1]
          const id = filename.split("/")[2]
          //log("JS: Opening ID: ", id)
          if (pathCategory === "data") {
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
          } else {
            console.log("JS: Invalid path category: ", pathCategory)
            return 0
          }
        },
        async read(fd, raw_dst_ptr, raw_length) {
          let to_read = Number(raw_length)
          let dst_ptr = Number(raw_dst_ptr)
          let stream = 0
          for (let i = 0; i < FS.streams.length; i++) {
            if (FS.streams[i].fd === fd) stream = FS.streams[i]
          }

          // Satisfy what we can with the cache first
          let bytes_read = this.readFromCache(stream, dst_ptr, to_read)
          stream.position += bytes_read
          stream.lastReadPosition = stream.position
          dst_ptr += bytes_read
          to_read -= bytes_read

          // Return if we have satisfied the request
          if (to_read === 0) {
            //console.log("WeaveDrive: Satisfied request with cache. Returning...")
            return bytes_read
          }
          //console.log("WeaveDrive: Read from cache: ", bytes_read, " Remaining to read: ", to_read)

          const chunk_download_sz = Math.max(to_read, CACHE_SZ)
          const to = Math.min(
            stream.node.total_size,
            stream.position + chunk_download_sz,
          )
          const data = await ar.data(stream.node.name)
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
            },
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
                //console.log("WeaveDrive: Writing: ", write_length, " bytes to: ", dst_ptr)
                mod.HEAP8.set(chunk_bytes.subarray(0, write_length), dst_ptr)
                dst_ptr += write_length
                bytes_read += write_length
                stream.position += write_length
                to_read -= write_length
              }

              if (to_read == 0) {
                // Add excess bytes to our cache
                const chunk_to_cache = chunk_bytes.subarray(write_length)
                //console.log("WeaveDrive: Cacheing excess: ", chunk_to_cache.length)
                cache_chunks.push(chunk_to_cache)
              }

              if (bytes_until_cache <= 0) {
                console.log(
                  "WeaveDrive: Chunk size reached. Compressing cache...",
                )
                stream.node.cache = this.addChunksToCache(
                  stream.node.cache,
                  cache_chunks,
                )
                cache_chunks = []
                bytes_until_cache = CHUNK_SZ
              }

              if (bytes_until_notify <= 0) {
                console.log(
                  "WeaveDrive: Downloaded: ",
                  (downloaded_bytes / stream.node.total_size) * 100,
                  "%",
                )
                bytes_until_notify = NOTIFY_SZ
              }
            }
          } catch (error) {
            console.error("WeaveDrive: Error reading the stream: ", error)
          } finally {
            reader.releaseLock()
          }
          // If we have no cache, or we have not satisfied the full request, we need to download the rest
          // Rebuild the cache from the new cache chunks
          stream.node.cache = this.addChunksToCache(
            stream.node.cache,
            cache_chunks,
          )

          // Update the last read position
          stream.lastReadPosition = stream.position
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
          if (stream.lastReadPosition !== stream.position) {
            //console.log("WeaveDrive: Invalidating cache for fd: ", stream.fd, " Current pos: ", stream.position, " Last read pos: ", stream.lastReadPosition)
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
            CACHE_SZ,
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
                current_offset,
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
