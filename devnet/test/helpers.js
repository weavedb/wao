import { spawn } from "node:child_process"

export function startWrangler({ port = 8787, cwd, config, vars = {}, healthPath = "/ar", inspectorPort } = {}) {
  return new Promise((resolve, reject) => {
    const args = ["wrangler", "dev", "--port", String(port)]
    if (config) args.push("-c", config)
    // Use a unique inspector port to avoid collisions when running multiple instances
    const iPort = inspectorPort ?? (9229 + (port - 8787))
    args.push("--inspector-port", String(iPort))
    for (const [k, v] of Object.entries(vars)) {
      args.push("--var", `${k}:${v}`)
    }
    const proc = spawn("npx", args, {
      cwd,
      stdio: ["ignore", "pipe", "pipe"],
      env: { ...process.env, NO_COLOR: "1" },
    })

    let stdout = ""
    let stderr = ""
    proc.stdout.on("data", (d) => (stdout += d.toString()))
    proc.stderr.on("data", (d) => (stderr += d.toString()))

    proc.on("error", (err) => reject(err))
    const baseUrl = `http://localhost:${port}`
    const maxWait = 120_000
    const start = Date.now()

    const poll = async () => {
      while (Date.now() - start < maxWait) {
        try {
          const res = await fetch(`${baseUrl}${healthPath}`)
          if (res.ok) {
            return resolve({
              baseUrl,
              kill() {
                return new Promise((res) => {
                  proc.on("exit", () => res())
                  proc.kill("SIGTERM")
                  setTimeout(() => {
                    try { proc.kill("SIGKILL") } catch {}
                  }, 5000)
                })
              },
            })
          }
        } catch {}
        await new Promise((r) => setTimeout(r, 1000))
      }
      proc.kill("SIGKILL")
      reject(new Error(`Wrangler failed to start within ${maxWait}ms.\nstdout: ${stdout}\nstderr: ${stderr}`))
    }

    poll()
  })
}
