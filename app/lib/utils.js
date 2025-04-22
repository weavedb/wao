import { useEffect, useState } from "react"
import { hash as sha256 } from "fast-sha256"
import { splitEvery, fromPairs, map, filter, includes } from "ramda"
import { common, createStarryNight } from "@wooorm/starry-night"
import markdownIt from "markdown-it"
import * as cheerio from "cheerio"
import { toHtml } from "hast-util-to-html"
import dayjs from "dayjs"
import relativeTime from "dayjs/plugin/relativeTime"
import { v4 } from "uuid"
import g from "/lib/global"

dayjs.extend(relativeTime)

function generateId() {
  return v4()
}

const wait = ms => new Promise(res => setTimeout(() => res(), ms))

const tags = tags => fromPairs(map(v => [v.name, v.value])(tags))

const getPreview = async txt => {
  const starryNight = await createStarryNight(common)
  const markdownItInstance = markdownIt({
    html: true,
    highlight(value, lang) {
      const scope = starryNight.flagToScope(lang)
      return toHtml({
        type: "element",
        tagName: "pre",
        properties: {
          className: scope
            ? [
                "highlight",
                "highlight-" +
                  scope.replace(/^source\./, "").replace(/\./g, "-"),
              ]
            : undefined,
        },
        children: scope
          ? /** @type {Array<ElementContent>} */ (
              starryNight.highlight(value, scope).children
            )
          : [{ type: "text", value }],
      })
    },
  })
  const html = markdownItInstance.render(txt)
  const $ = cheerio.load(html)
  $("a").each((_, el) => {
    const href = $(el).attr("href")
    $(el).attr("target", "_blank")
    if (href && !href.match(/^(?:[a-z]+:)?\/\//i)) {
      $(el).addClass("relative-link")
      $(el).attr("data-href", href)
    }
  })

  return $.html()
}

function useResizeObserver(ref) {
  const [dimensions, setDimensions] = useState({ width: 0, height: 0 })

  useEffect(() => {
    // Only run on client side
    if (typeof window === "undefined" || !ref.current) return

    const observeTarget = ref.current

    const resizeObserver = new ResizeObserver(entries => {
      // We only care about the first element, most often there is only one.
      const entry = entries[0]

      setDimensions({
        width: entry.contentRect.width,
        height: entry.contentRect.height,
      })
    })

    resizeObserver.observe(observeTarget)

    return () => {
      resizeObserver.unobserve(observeTarget)
      resizeObserver.disconnect()
    }
  }, [ref])

  return dimensions
}

function resolvePath(basePath, relativePath) {
  if (relativePath[0] === "/") return relativePath
  const stack = basePath.replace(/\/+$/, "").split("/")
  const parts = relativePath.split("/")

  for (const part of parts) {
    if (part === "..") {
      if (stack.length > 1) stack.pop()
    } else if (part !== "." && part !== "") {
      stack.push(part)
    }
  }

  return stack.join("/")
}

const ftype = fileext => {
  return fileext === "js"
    ? "javascript"
    : fileext === "ts"
      ? "typescript"
      : fileext === "md"
        ? "markdown"
        : fileext
}

const getAct = message => {
  if (message) {
    const t = tags(message.http_msg.tags)
    return t.Type === "Process" ? "Process" : t.Action
  }
  return null
}

const filterFiles = filter(v => {
  return v.pid && !includes(v.pid, ["0", "2", "3"])
})
const filterProjects = filter(v => v.id !== "2")
const short = (addr, len = 8) => {
  return !addr ? "" : addr.slice(0, len) + "..." + addr.slice(-len)
}
const fromNow = ts => {
  return dayjs(ts).fromNow()
}

function b64urlEncode(buf) {
  const b64 =
    typeof btoa === "function"
      ? btoa(String.fromCharCode(...buf))
      : Buffer.from(buf).toString("base64")
  return b64.replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "")
}
function b64urlDecode(str) {
  str = str.replace(/-/g, "+").replace(/_/g, "/")
  if (str.length % 4) str += "=".repeat(4 - (str.length % 4))
  if (typeof atob === "function") {
    const bin = atob(str)
    return Uint8Array.from(bin, c => c.charCodeAt(0))
  }
  return Uint8Array.from(Buffer.from(str, "base64"))
}

const toAddr = pub => b64urlEncode(sha256(b64urlDecode(pub)))

const k = {
  down: (n = 0) => `\x1b[${n === 0 ? "" : n}B`,
  up: (n = 0) => `\x1b[${n === 0 ? "" : n}A`,
  right: (n = 0) => `\x1b[${n === 0 ? "" : n}C`,
  left: (n = 0) => `\x1b[${n === 0 ? "" : n}D`,
  x: (n = 0) => `\x1b[${n === 0 ? "" : n}G`,
  ctrlA: "\x01",
  ctrlE: "\x05",
  ctrlF: "\x06",
  ctrlB: "\x02",
  del: "\x1b[3~",
  ctrlK: "\x0b",
  ctrlV: "\x16",
  ctrlD: "\x04",
  altD: "\x1bd",
  enter: 13,
  bs: 127,
  moveL: () => {
    if (g.cur > 0) {
      const cursorX = g.term.buffer.active.cursorX
      if (cursorX === 0 && g.cur > 0) {
        const cols = g.term.cols
        g.term.write(k.up())
        g.term.write(k.x(cols))
      } else g.term.write(k.left())
      g.cur--
    }
  },
  moveR: () => {
    if (g.cur < g.inputRef.current.length) {
      const cursorX = g.term.buffer.active.cursorX
      const cols = g.term.cols
      if (cursorX === cols - 1) {
        g.term.write(k.down())
        g.term.write("\r")
      } else g.term.write(k.right())
      g.cur++
    }
  },
  stats: () => {
    const cols = g.term.cols
    const x = g.term.buffer.active.cursorX
    const len = g.inputRef.current.length
    const plen = g.plen
    let y = 0
    if (g.cur > x) y = Math.floor((g.cur + plen - x) / cols)
    const rows = Math.ceil((plen + len) / cols)
    return { cols, x, len, y, plen, rows, cur: g.cur }
  },
  backspace: () => {
    if (g.cur > 0) {
      k.moveL()
      setTimeout(() => k.delete(), 0)
    }
  },
  delete: () => {
    const { x, y, rows, cur, len } = k.stats()
    if (cur < len) {
      const left = g.inputRef.current.slice(0, g.cur)
      const right = g.inputRef.current.slice(g.cur + 1)
      g.inputRef.current = left + right
      g.term.write(right + " ")
      g.term.write(k.left(right.length + 1))
      g.term.write(k.x(x + 1))
      if (rows - y - 1 > 0) g.term.write(k.up(rows - y - 1))
    }
  },
  void: () => {},
  downH: () => {
    if (g.historyIndex < g.history.length - 1) {
      g.historyIndex++
      k.history(g.history[g.historyIndex])
    } else if (g.historyIndex === g.history.length - 1) {
      g.historyIndex++
      k.history(g.savedInput ?? "")
      g.savedInput = null
    }
  },
  upH: () => {
    if (g.history.length > 0) {
      if (g.historyIndex === g.history.length) {
        g.savedInput = g.inputRef.current
      }
      if (g.historyIndex > 0) {
        g.historyIndex--
        k.history(g.history[g.historyIndex])
      }
    }
  },
  history: newInput => {
    const oldLen = g.inputRef.current.length
    const newLen = newInput.length
    const cur = g.cur
    if (cur > 0) g.term.write(k.left(cur))
    g.term.write(" ".repeat(oldLen))
    if (oldLen > 0) g.term.write(k.left(oldLen))
    g.inputRef.current = newInput
    g.cur = newLen
    g.term.write(newInput)
  },
  cutW: () => {
    if (g.cur < g.inputRef.current.length) {
      const isWordChar = c => /\w/.test(c)
      const text = g.inputRef.current
      let end = g.cur
      while (end < text.length && text[end] === " ") end++
      const first = isWordChar(text[end])
      while (
        end < text.length &&
        (isWordChar(text[end]) === first || (!first && text[end] === " "))
      ) {
        end++
      }
      const left = text.slice(0, g.cur)
      const right = text.slice(end)
      g.inputRef.current = left + right
      g.term.write(right + " ".repeat(end - g.cur))
      g.term.write(k.left(right.length + (end - g.cur)))
    }
  },
  cut: () => {
    const { x, y, rows } = k.stats()
    const left = g.inputRef.current.slice(0, g.cur)
    const right = g.inputRef.current.slice(g.cur)
    g.inputRef.current = left
    g.term.write(" ".repeat(right.length))
    g.term.write(k.x(x + 1))
    if (rows - y - 1 > 0) g.term.write(k.up(rows - y - 1))
  },
  write: txt => {
    const { len, x, cols, y, plen, rows } = k.stats()
    const left = g.inputRef.current.slice(0, g.cur)
    const right = g.inputRef.current.slice(g.cur)
    let txt2 = txt + right
    let rlen = right.length
    g.term.write(txt2)
    if (rlen > 0) {
      const get = (x, txt) => {
        const pad = cols - x
        let x2 = 0
        let up = 0
        if (txt.length <= pad) {
          x2 = x + txt.length
        } else {
          let txt3 = txt.slice(pad)
          while (txt3.length > 0) {
            up++
            x2 = txt3.length
            txt3 = txt3.slice(cols)
          }
        }
        return { newx: x2, up }
      }
      let { newx } = get(x, txt)
      let { up } = get(newx, right)
      if (x === cols - 1 && txt.length === 1) {
        newx = -1
        up -= 1
      }
      g.term.write(k.x(newx + 1))
      if (up > 0) g.term.write(k.up(up))
      if (up < 0) g.term.write(k.down(up * -1))
    }

    g.inputRef.current = left + txt + right
    g.cur += txt.length
  },
  paste: () => {
    navigator.clipboard
      .readText()
      .then(paste => {
        k.write(paste)
      })
      .catch(err => {
        console.error("Clipboard read failed:", err)
      })
  },
  end: () => {
    if (g.cur < g.inputRef.current.length) {
      const { len, x, cols, y, plen, rows } = k.stats()
      const lastLine = (plen + len) % cols
      const linesDown = rows - y - 1
      if (linesDown > 0) g.term.write(k.down(linesDown))
      if (lastLine - x > 0) g.term.write(k.right(lastLine - x))
      else if (lastLine - x < 0) g.term.write(k.left(x - lastLine))
      g.cur = len
    }
  },
  top: () => {
    if (g.cur > 0) {
      const { plen, len, x, cols, y } = k.stats()
      if (g.cur < x) g.term.write(k.left(g.cur))
      else {
        if (y > 0) g.term.write(k.up(y))
        if (plen - x > 0) g.term.write(k.right(plen - x))
        else if (plen - x < 0) g.term.write(k.left(x - plen))
      }
      g.cur = 0
    }
  },
}
export {
  k,
  toAddr,
  fromNow,
  short,
  filterProjects,
  filterFiles,
  getAct,
  ftype,
  wait,
  generateId,
  tags,
  useResizeObserver,
  getPreview,
  dayjs,
  resolvePath,
}
