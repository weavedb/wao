export default {
  // fundamental
  meta: { name: "meta@1.0", module: "dev_meta" },
  json: { name: "json@1.0", module: "dev_codec_json" },
  flat: { name: "flat@1.0", module: "dev_codec_flat" },
  httpsig: { name: "httpsig@1.0", module: "dev_codec_httpsig" },
  structured: { name: "structured@1.0", module: "dev_codec_structured" },
  wao: { name: "wao@1.0", module: "dev_wao" },

  // process
  process: { name: "process@1.0", module: "dev_process" },
  message: { name: "message@1.0", module: "dev_message" },
  scheduler: { name: "scheduler@1.0", module: "dev_scheduler" },

  // legacynet aos
  "delegated-compute": {
    name: "delegated-compute@1.0",
    module: "dev_delegated_compute",
  },
  "genesis-wasm": { name: "genesis-wasm@1.0", module: "dev_genesis_wasm" },

  // hyper aos
  lua: { name: "lua@5.3a", module: "dev_lua" },

  // mainnet aos
  wasi: { name: "wasi@1.0", module: "dev_wasi" },
  "wasm-64": { name: "wasm-64@1.0", module: "dev_wasm" },
  "json-iface": { name: "json-iface@1.0", module: "dev_json_iface" },

  // process utils
  "test-device": { name: "test-device@1.0", module: "dev_test" },
  patch: { name: "patch@1.0", module: "dev_patch" },
  push: { name: "push@1.0", module: "dev_push" },
  stack: { name: "stack@1.0", module: "dev_stack" },
  multipass: { name: "multipass@1.0", module: "dev_multipass" },

  // payment
  faff: { name: "faff@1.0", module: "dev_faff" },
  p4: { name: "p4@1.0", module: "dev_p4" },
  "node-process": { name: "node-process@1.0", module: "dev_node_process" },
  "simple-pay": { name: "simple-pay@1.0", module: "dev_simple_pay" },

  // tested
  cron: { name: "cron@1.0", module: "dev_cron" },
  relay: { name: "relay@1.0", module: "dev_relay" },
  router: { name: "router@1.0", module: "dev_router" },

  // storage
  cache: { name: "cache@1.0", module: "dev_cache" },
  "local-name": { name: "local-name@1.0", module: "dev_local_name" },
  lookup: { name: "lookup@1.0", module: "dev_lookup" },
  name: { name: "name@1.0", module: "dev_name" },

  // others
  compute: { name: "compute@1.0", module: "dev_cu" },
  dedup: { name: "dedup@1.0", module: "dev_dedup" },
  manifest: { name: "manifest@1.0", module: "dev_manifest" },
  monitor: { name: "monitor@1.0", module: "dev_monitor" },

  // advanced
  snp: { name: "snp@1.0", module: "dev_snp" },
  volume: { name: "volume@1.0", module: "dev_volume" },
  poda: { name: "poda@1.0", module: "dev_poda" },
  greenzone: { name: "greenzone@1.0", module: "dev_green_zone" },

  // misc
  hyperbuddy: { name: "hyperbuddy@1.0", module: "dev_hyperbuddy" },
  ans104: { name: "ans104@1.0", module: "dev_codec_ans104" },
  cacheviz: { name: "cacheviz@1.0", module: "dev_cacheviz" },
}
