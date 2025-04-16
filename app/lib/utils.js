import { useEffect, useState } from "react"
import { fromPairs, map, filter, includes } from "ramda"
import { common, createStarryNight } from "@wooorm/starry-night"
import markdownIt from "markdown-it"
import * as cheerio from "cheerio"
import { toHtml } from "hast-util-to-html"
import dayjs from "dayjs"
import relativeTime from "dayjs/plugin/relativeTime"
import { v4 } from "uuid"
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
const DateMS = Date

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
export {
  filterProjects,
  filterFiles,
  getAct,
  DateMS,
  ftype,
  wait,
  generateId,
  tags,
  useResizeObserver,
  getPreview,
  dayjs,
  resolvePath,
}
