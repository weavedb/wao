import { defineConfig } from "vite"
import { viteSingleFile } from "vite-plugin-singlefile"

export default defineConfig({
  root: "app",
  plugins: [viteSingleFile()],
  build: { outDir: "../dist", emptyOutDir: true },
  server: {
    proxy: {
      "/ar": "http://localhost:8787",
      "/su": "http://localhost:8787",
      "/cu": "http://localhost:8787",
      "/mu": "http://localhost:8787",
      "/bd": "http://localhost:8787",
    },
  },
})
