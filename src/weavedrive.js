const KB = 1024
const MB = KB * 1024
const CACHE_SZ = 32 * KB
const CHUNK_SZ = 128 * MB
const NOTIFY_SZ = 512 * MB
const log = console.log

export default class WeaveDrive {
  constructor(ar) {
    this.drive = function WeaveDrive(mod, FS) {
      return {
        reset(fd) {
          //console.log("WeaveDrive: Resetting fd: ", fd)
          FS.streams[fd].node.position = 0
          FS.streams[fd].node.cache = new Uint8Array(0)
        },

        async create(id) {
          var properties = { isDevice: false, contents: null }

          if (!(await this.checkAdmissible(id))) {
            //console.log("WeaveDrive: Arweave ID is not admissable! ", id)
            return 0
          }

          // Create the file in the emscripten FS

          if (!FS.analyzePath("/data/").exists) FS.mkdir("/data/")

          var node = FS.createFile("/", "data/" + id, properties, true, false)
          // Set initial parameters
          /*
            var bytesLength = await this.customFetch(`/${id}`, {
            method: "HEAD",
            }).then(res => res.headers.get("Content-Length"))
          */
          let data = await ar.data(id)
          const bytesLength = data?.length ?? 0
          node.total_size = Number(bytesLength)
          node.cache = new Uint8Array(0)
          node.position = 0

          // Add a function that defers querying the file size until it is asked the first time.
          Object.defineProperties(node, {
            usedBytes: {
              get: function () {
                return bytesLength
              },
            },
          })

          // Now we have created the file in the emscripten FS, we can open it as a stream
          var stream = FS.open("/data/" + id, "r")

          //console.log("JS: Created file: ", id, " fd: ", stream.fd);
          return stream
        },
        async createBlockHeader(id) {
          var result = ""
          try {
            // todo: implement indep_hash
            // fetch(`/block/height/${id}`)
            const block = ar.mem.blockmap[ar.mem.blocks[id]]
            if (block) {
              result = JSON.stringify({
                id: block.id,
                timestamp: block.timestamp,
                previous_block: block.previous,
                txs: block.txs,
                height: id,
              })
            }
          } catch (e) {}

          var bytesLength = result.length

          var node = FS.createDataFile(
            "/",
            "block/" + id,
            Buffer.from(result, "utf-8"),
            true,
            false,
          )

          var stream = FS.open("/block/" + id, "r")
          return stream
        },
        async createTxHeader(id) {
          var result = ""
          // fetch(`/tx/${id}`)
          try {
            let tx = ar.mem.txs[id]
            if (tx) result = JSON.stringify(tx)
          } catch (e) {}
          var node = FS.createDataFile(
            "/",
            "tx/" + id,
            Buffer.from(result, "utf-8"),
            true,
            false,
          )
          var stream = FS.open("/tx/" + id, "r")
          return stream
        },
        async createDataItemTxHeader(id) {
          let result = (
            await ar.gql.txs({
              id,
              fields: [
                "id",
                "anchor",
                { data: ["size"] },
                "signature",
                "recipient",
                "owner",
                "fee",
                "quantity",
                "tags",
                "bundledIn",
                "block",
              ],
            })
          )[0]
          result = result ? JSON.stringify(result) : "No results"
          FS.createDataFile(
            "/",
            "tx2/" + id,
            Buffer.from(result, "utf-8"),
            true,
            false,
          )
          var stream = FS.open("/tx2/" + id, "r")

          return stream
        },
        async open(filename) {
          const pathCategory = filename.split("/")[1]
          const id = filename.split("/")[2]
          console.log("JS: Opening ID: ", id)
          if (pathCategory === "tx") {
            FS.createPath("/", "tx", true, false)
            if (FS.analyzePath(filename).exists) {
              var stream = FS.open(filename, "r")
              if (stream.fd) return stream.fd
              return 0
            } else {
              const stream = await this.createTxHeader(id)
              return stream.fd
            }
          }
          if (pathCategory === "tx2") {
            FS.createPath("/", "tx2", true, false)
            if (FS.analyzePath(filename).exists) {
              var stream = FS.open(filename, "r")
              if (stream.fd) return stream.fd
              return 0
            } else {
              const stream = await this.createDataItemTxHeader(id)
              if (stream.fd) return stream.fd
              return 0
            }
          }
          if (pathCategory === "block") {
            FS.createPath("/", "block", true, false)
            if (FS.analyzePath(filename).exists) {
              var stream = FS.open(filename, "r")
              if (stream.fd) return stream.fd
              return 0
            } else {
              const stream = await this.createBlockHeader(id)
              return stream.fd
            }
          }
          if (pathCategory === "data") {
            if (FS.analyzePath(filename).exists) {
              var stream = FS.open(filename, "r")
              if (stream.fd) return stream.fd
              console.log("JS: File not found: ", filename)
              return 0
            } else {
              //console.log("JS: Open => Creating file: ", id);
              const stream = await this.create(id)
              //console.log("JS: Open => Created file: ", id, " fd: ", stream.fd);
              return stream.fd
            }
          } else if (pathCategory === "headers") {
            console.log("Header access not implemented yet.")
            return 0
          } else {
            console.log("JS: Invalid path category: ", pathCategory)
            return 0
          }
        },
        async read(fd, raw_dst_ptr, raw_length) {
          // Note: The length and dst_ptr are 53 bit integers in JS, so this _should_ be ok into a large memspace.
          var to_read = Number(raw_length)
          var dst_ptr = Number(raw_dst_ptr)

          var stream = 0
          for (var i = 0; i < FS.streams.length; i++) {
            if (FS.streams[i].fd === fd) {
              stream = FS.streams[i]
            }
          }
          // read block headers
          if (stream.path.includes("/block")) {
            mod.HEAP8.set(stream.node.contents.subarray(0, to_read), dst_ptr)
            return to_read
          }
          // read tx headers
          if (stream.path.includes("/tx")) {
            mod.HEAP8.set(stream.node.contents.subarray(0, to_read), dst_ptr)
            return to_read
          }
          // Satisfy what we can with the cache first
          var bytes_read = this.readFromCache(stream, dst_ptr, to_read)
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
          //console.log("WeaveDrive: fd: ", fd, " Read length: ", to_read, " Reading ahead:", to - to_read - stream.position)

          // fetch(`/${stream.node.name}`)
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
          var bytes_until_cache = CHUNK_SZ
          var bytes_until_notify = NOTIFY_SZ
          var downloaded_bytes = 0
          var cache_chunks = []

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
          var stream = 0
          for (var i = 0; i < FS.streams.length; i++) {
            if (FS.streams[i].fd === fd) stream = FS.streams[i]
          }
          FS.close(stream)
        },

        // Readahead cache functions
        readFromCache(stream, dst_ptr, length) {
          // Check if the cache has been invalidated by a seek
          if (stream.lastReadPosition !== stream.position) {
            //console.log("WeaveDrive: Invalidating cache for fd: ", stream.fd, " Current pos: ", stream.position, " Last read pos: ", stream.lastReadPosition)
            stream.node.cache = new Uint8Array(0)
            return 0
          }
          // Calculate the bytes of the request that can be satisfied with the cache
          var cache_part_length = Math.min(length, stream.node.cache.length)
          var cache_part = stream.node.cache.subarray(0, cache_part_length)
          mod.HEAP8.set(cache_part, dst_ptr)
          // Set the new cache to the remainder of the unused cache and update pointers
          stream.node.cache = stream.node.cache.subarray(cache_part_length)

          return cache_part_length
        },

        addChunksToCache(old_cache, chunks) {
          // Make a new cache array of the old cache length + the sum of the chunk lengths, capped by the max cache size
          var new_cache_length = Math.min(
            old_cache.length +
              chunks.reduce((acc, chunk) => acc + chunk.length, 0),
            CACHE_SZ,
          )
          var new_cache = new Uint8Array(new_cache_length)
          // Copy the old cache to the new cache
          new_cache.set(old_cache, 0)
          // Load the cache chunks into the new cache
          var current_offset = old_cache.length
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

        // General helpder functions
        async checkAdmissible(ID) {
          // CAUTION: If the module is initiated with `mode = test` we don't check availability.
          if (mod.mode && mod.mode == "test") return true

          // Check if we are attempting to load the On-Boot id, if so allow it
          // this was added for AOP 6 Boot loader See: https://github.com/permaweb/aos/issues/342
          const bootTag = this.getTagValue("On-Boot", mod.spawn.tags)
          if (bootTag && bootTag === ID) return true

          // Check that this module or process set the WeaveDrive tag on spawn
          const blockHeight = mod.blockHeight
          const moduleExtensions = this.getTagValues(
            "Extension",
            mod.module.tags,
          )
          const moduleHasWeaveDrive = moduleExtensions.includes("WeaveDrive")

          const processExtensions = this.getTagValues(
            "Extension",
            mod.spawn.tags,
          )
          const processHasWeaveDrive =
            moduleHasWeaveDrive || processExtensions.includes("WeaveDrive")

          if (!processHasWeaveDrive) {
            console.log(
              "WeaveDrive: Process tried to call WeaveDrive, but extension not set!",
            )
            return false
          }
          const modes = ["Assignments", "Individual", "Library"]
          // Get the Availability-Type from the spawned process's Module or Process item
          // First check the module for its defaults
          const moduleAvailabilityType = this.getTagValue(
            "Availability-Type",
            mod.module.tags,
          )
          const moduleMode = moduleAvailabilityType
            ? moduleAvailabilityType
            : "Assignments" // Default to assignments

          // Now check the process's spawn item. These settings override Module item settings.
          const processAvailabilityType = this.getTagValue(
            "Availability-Type",
            mod.spawn.tags,
          )
          const processMode = processAvailabilityType
            ? processAvailabilityType
            : moduleMode

          if (!modes.includes(processMode)) {
            throw `Unsupported WeaveDrive mode: ${processMode}`
          }

          const attestors = this.serializeStringArr(
            [
              this.getTagValue("Scheduler", mod.spawn.tags),
              ...this.getTagValues("Attestor", mod.spawn.tags),
            ].filter(t => !!t),
          )
          // Init a set of GraphQL queries to run in order to find a valid attestation
          // Every WeaveDrive process has at least the "Assignments" availability check form.
          const exists = async tags => {
            const common = {
              owners: attestors,
              block: [0, blockHeight],
              fields: ["tags"],
            }
            return (await ar.gql.txs({ ...common, tags })).length > 0
          }
          const assignmentsHaveID = await exists({
            Type: "Attestation",
            Message: ID,
            "Data-Protocol": "ao",
          })
          if (assignmentsHaveID) return true

          if (processMode == "Individual") {
            const individualsHaveID = await exists({
              Type: "Available",
              ID,
              "Data-Protocol": "WeaveDrive",
            })
            if (individualsHaveID) return true
          }

          if (processMode == "Library") {
            throw "This WeaveDrive implementation does not support Library attestations yet!"
          }

          return false
        },

        serializeStringArr(arr = []) {
          return `[${arr.map(s => `"${s}"`).join(", ")}]`
        },

        getTagValues(key, tags) {
          var values = []
          for (let i = 0; i < tags.length; i++) {
            if (tags[i].name == key) values.push(tags[i].value)
          }
          return values
        },

        getTagValue(key, tags) {
          const values = this.getTagValues(key, tags)
          return values.pop()
        },
      }
    }
  }
}
