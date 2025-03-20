const KB = 1024
const MB = KB * 1024
const CACHE_SZ = 32 * KB // Keep small cache size
const CHUNK_SZ = 1 * MB // Reduced from 128MB to 1MB for better memory management
const STREAM_CHUNK = 256 * KB // Added smaller streaming chunk size
const log = console.log
let init = {}
export default class WeaveDrive {
  constructor(ar) {
    this.ext = (mod, FS) => {
      return {
        reset(fd) {
          FS.streams[fd].node.position = 0
          FS.streams[fd].node.cache = new Uint8Array(0)
        },

        async create(id) {
          if (!(await this.checkAdmissible(id))) return 0

          if (!FS.analyzePath("/data/").exists) FS.mkdir("/data/")

          var properties = { isDevice: false, contents: null }
          var node = FS.createFile("/", "data/" + id, properties, true, false)

          let bytesLength = 0
          try {
            let data = await ar.data(id)
            bytesLength = data?.length ?? 0
          } catch (e) {
            console.error("Failed to get data length:", e)
            return 0
          }

          node.total_size = Number(bytesLength)
          node.cache = new Uint8Array(0)
          node.position = 0
          node.chunks = new Map() // Add chunk cache

          Object.defineProperties(node, {
            usedBytes: { get: () => bytesLength },
          })

          var stream = FS.open("/data/" + id, "r")
          return stream
        },

        async createBlockHeader(id) {
          var result = ""
          try {
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

          var node = FS.createDataFile(
            "/",
            "block/" + id,
            Buffer.from(result, "utf-8"),
            true,
            false
          )

          var stream = FS.open("/block/" + id, "r")
          return stream
        },

        async createTxHeader(id) {
          var result = ""
          try {
            let tx = ar.mem.txs[id]
            if (tx) result = JSON.stringify(tx)
          } catch (e) {}
          var node = FS.createDataFile(
            "/",
            "tx/" + id,
            Buffer.from(result, "utf-8"),
            true,
            false
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
            false
          )
          var stream = FS.open("/tx2/" + id, "r")
          return stream
        },

        // Chunk management helpers
        getChunkKey(position) {
          return Math.floor(position / CHUNK_SZ)
        },

        async fetchChunk(stream, chunkKey) {
          const start = chunkKey * CHUNK_SZ
          const end = Math.min(start + CHUNK_SZ, stream.node.total_size)

          try {
            const data = await ar.data(stream.node.name)
            return data.subarray(start, end)
          } catch (e) {
            console.error("Failed to fetch chunk:", e)
            return new Uint8Array(0)
          }
        },

        async ensureChunkLoaded(stream, position) {
          const chunkKey = this.getChunkKey(position)

          if (!stream.node.chunks.has(chunkKey)) {
            // Remove old chunks if we have too many
            if (stream.node.chunks.size >= 3) {
              // Keep only 3 chunks in memory
              const oldestKey = stream.node.chunks.keys().next().value
              stream.node.chunks.delete(oldestKey)
            }

            const chunk = await this.fetchChunk(stream, chunkKey)
            stream.node.chunks.set(chunkKey, chunk)
          }

          return stream.node.chunks.get(chunkKey)
        },

        async read(fd, raw_dst_ptr, raw_length) {
          var to_read = Number(raw_length)
          var dst_ptr = Number(raw_dst_ptr)

          var stream = FS.streams.find(s => s.fd === fd)
          if (!stream) return 0
          if (init[fd] === false) stream.position = 0
          init[fd] = true
          // Handle block and tx headers
          if (stream.path.includes("/block") || stream.path.includes("/tx")) {
            mod.HEAP8.set(stream.node.contents.subarray(0, to_read), dst_ptr)
            return to_read
          }

          let bytesRead = 0
          while (to_read > 0) {
            // Get current chunk
            const chunk = await this.ensureChunkLoaded(stream, stream.position)
            const chunkOffset = stream.position % CHUNK_SZ

            // Calculate how much we can read from this chunk
            const available = chunk.length - chunkOffset
            const readSize = Math.min(to_read, available)

            if (readSize <= 0) break

            // Copy data to destination
            const data = chunk.subarray(chunkOffset, chunkOffset + readSize)
            mod.HEAP8.set(data, dst_ptr)

            // Update pointers
            stream.position += readSize
            dst_ptr += readSize
            to_read -= readSize
            bytesRead += readSize
          }

          return bytesRead
        },

        // Keep existing helper methods
        close(fd) {
          var stream = FS.streams.find(s => s.fd === fd)
          if (stream) {
            stream.node.chunks?.clear() // Clean up chunks
            FS.close(stream)
          }
        },

        // ... rest of the existing methods (checkAdmissible, getTagValues, etc.) ...
        async checkAdmissible(ID) {
          if (mod.mode && mod.mode == "test") return true

          const bootTag = this.getTagValue("On-Boot", mod.spawn.tags)
          if (bootTag && bootTag === ID) return true

          const blockHeight = mod.blockHeight
          const moduleExtensions = this.getTagValues(
            "Extension",
            mod.module.tags
          )
          const moduleHasWeaveDrive = moduleExtensions.includes("WeaveDrive")

          const processExtensions = this.getTagValues(
            "Extension",
            mod.spawn.tags
          )
          const processHasWeaveDrive =
            moduleHasWeaveDrive || processExtensions.includes("WeaveDrive")

          if (!processHasWeaveDrive) {
            console.log(
              "WeaveDrive: Process tried to call WeaveDrive, but extension not set!"
            )
            return false
          }

          const modes = ["Assignments", "Individual", "Library"]
          const moduleAvailabilityType = this.getTagValue(
            "Availability-Type",
            mod.module.tags
          )
          const moduleMode = moduleAvailabilityType
            ? moduleAvailabilityType
            : "Assignments"

          const processAvailabilityType = this.getTagValue(
            "Availability-Type",
            mod.spawn.tags
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
            ].filter(t => !!t)
          )

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
              const stream = await this.create(id)
              init[stream.fd] = false
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
      }
    }
  }
}
