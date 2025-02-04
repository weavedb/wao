const arnext = require("arnext/config")
const nextConfig = {
  reactStrictMode: true,
  experimental: {
    optimizePackageImports: ["@chakra-ui/react"],
  },
}
module.exports = arnext(nextConfig)
