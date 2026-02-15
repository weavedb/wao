#!/usr/bin/env node

// MCP server for WAO Dashboard — JSON-RPC over stdio
// Provides get_progress and open_dashboard tools to Claude Code

import { readFileSync, existsSync } from "node:fs"
import { join } from "node:path"
import { createInterface } from "node:readline"

const ROOT = join(import.meta.dirname, "../../..")

const TOOLS = [
  {
    name: "get_progress",
    description: "Get current build progress from tasks.json — returns feature name, tracks, task list with status, and current step.",
    inputSchema: { type: "object", properties: {}, required: [] },
  },
  {
    name: "open_dashboard",
    description: "Get the URLs to open the WAO build progress dashboard.",
    inputSchema: { type: "object", properties: {}, required: [] },
  },
]

function readProgress() {
  const tasksPath = join(ROOT, "tasks.json")
  if (!existsSync(tasksPath)) return { feature: null, tasks: [], tracks: [], current_step: 0 }
  try {
    return JSON.parse(readFileSync(tasksPath, "utf8"))
  } catch {
    return { feature: null, tasks: [], tracks: [], current_step: 0 }
  }
}

function handleRequest(msg) {
  const { id, method, params } = msg

  if (method === "initialize") {
    return {
      jsonrpc: "2.0", id,
      result: {
        protocolVersion: "2024-11-05",
        capabilities: { tools: {} },
        serverInfo: { name: "wao-dashboard", version: "1.0.0" },
      },
    }
  }

  if (method === "notifications/initialized") return null // no response needed

  if (method === "ping") {
    return { jsonrpc: "2.0", id, result: {} }
  }

  if (method === "tools/list") {
    return { jsonrpc: "2.0", id, result: { tools: TOOLS } }
  }

  if (method === "tools/call") {
    const toolName = params?.name
    if (toolName === "get_progress") {
      const data = readProgress()
      return {
        jsonrpc: "2.0", id,
        result: {
          content: [{ type: "text", text: JSON.stringify(data, null, 2) }],
        },
      }
    }
    if (toolName === "open_dashboard") {
      return {
        jsonrpc: "2.0", id,
        result: {
          content: [{
            type: "text",
            text: [
              "WAO Dashboard URLs:",
              "  Dev server:  http://localhost:5174",
              "  API server:  http://localhost:3333",
              "",
              "Start both with: yarn start",
              "Start API only:  yarn start:api",
            ].join("\n"),
          }],
        },
      }
    }
    return {
      jsonrpc: "2.0", id,
      error: { code: -32601, message: `Unknown tool: ${toolName}` },
    }
  }

  // Unknown method
  if (id !== undefined) {
    return {
      jsonrpc: "2.0", id,
      error: { code: -32601, message: `Method not found: ${method}` },
    }
  }
  return null
}

// --- stdio transport ---
const rl = createInterface({ input: process.stdin })
let buffer = ""

process.stdin.on("data", (chunk) => {
  buffer += chunk.toString()
  let newlineIdx
  while ((newlineIdx = buffer.indexOf("\n")) !== -1) {
    const line = buffer.slice(0, newlineIdx).trim()
    buffer = buffer.slice(newlineIdx + 1)
    if (!line) continue
    try {
      const msg = JSON.parse(line)
      const response = handleRequest(msg)
      if (response) {
        process.stdout.write(JSON.stringify(response) + "\n")
      }
    } catch {
      // ignore malformed JSON
    }
  }
})

// Keep process alive
process.stdin.resume()
