import {
  rmdirSync,
  unlinkSync,
  writeFileSync,
  readFileSync,
  readdirSync,
} from "fs"

import { resolve, extname } from "path"
import * as cheerio from "cheerio"

const out = resolve(import.meta.dirname, "out")
const dirs = readdirSync(out, { withFileTypes: true, recursive: true })

const prefixWithPath = url => {
  const cleanedUrl = url.replace(/^\.\//, "").replace(/^\//, "")
  const txt = "${path}" + cleanedUrl
  return "`" + txt + "`"
}

const isRelativeUrl = url => !/^https?:\/\//i.test(url)

let _dirs = []

for (const v of dirs) {
  const ext = extname(v.name)
  const htmlPath = resolve(v.path, v.name)
  if (v.isFile() && ext === ".html") {
    if (v.name !== "index.html") {
      unlinkSync(htmlPath)
    } else {
      const txt = readFileSync(htmlPath, "utf8")
      const $ = cheerio.load(txt)
      let _tags = []
      $("link[href]").each((i, elem) => {
        const href = $(elem).attr("href")
        if (isRelativeUrl(href)) {
          const props = { ...elem.attribs }
          props.href = prefixWithPath(href)
          _tags.push({ elm: "link", props })
          $(elem).remove()
        }
      })

      $("script[src]").each((i, elem) => {
        const src = $(elem).attr("src")
        if (isRelativeUrl(src)) {
          const props = { ...elem.attribs }
          props.src = prefixWithPath(src)
          _tags.push({ elm: "script", props })
          $(elem).remove()
        }
      })

      $("script").each((i, elem) => {
        const scriptContent = $(elem).html()
        if (scriptContent && scriptContent.includes("let tags = []")) {
          const tagsString = JSON.stringify(_tags)
          const updatedScriptContent = scriptContent
            .replace(/let\s+tags\s*=\s*\[\s*\]/, `let tags = ${tagsString}`)
            .replace(/"`/g, "`")
            .replace(/`"/g, "`")
          $(elem).html(updatedScriptContent)
        }
      })
      const modifiedHtml = $.root().html()
      writeFileSync(htmlPath, modifiedHtml)
    }
  } else if (v.isFile() && ext === ".js" && /^webpack/.test(v.name)) {
    const txt = readFileSync(htmlPath, "utf8")
    const x = txt.match(/(.)\.p="\/_next\/"/)[1]
    const mod = txt.replace(
      new RegExp(`o=${x}\.p`, "g"),
      `o=_assetPath()+${x}.p`,
    )
    writeFileSync(htmlPath, mod)
  }
}

const dirs2 = readdirSync(out, { withFileTypes: true })
for (const v of dirs2) {
  const htmlPath = resolve(v.path, v.name)
  console.log(htmlPath)
  if (v.isDirectory() && v.name !== "_next") {
    rmdirSync(htmlPath, { recursive: true, force: true })
  }
}
