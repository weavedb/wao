import { defineConfig } from "vocs"

const ar = [
  "rsa_pss",
  "ar_wallet",
  "ar_bundles",
  "ar_tx",
  "ar_deep_hash",
  "ar_timestamp",
  "ar_rate_limiter",
]
const hb = [
  "hb",
  "hb_ao",
  "hb_ao_device",
  "hb_ao_test_vectors",
  "hb_app",
  "hb_beamr",
  "hb_beamr_io",
  "hb_cache",
  "hb_cache_control",
  "hb_cache_render",
  "hb_client",
  "hb_crypto",
  "hb_debugger",
  "hb_escape",
  "hb_event",
  "hb_examples",
  "hb_features",
  "hb_format",
  "hb_gateway_client",
  "hb_http",
  "hb_http_benchmark_tests",
  "hb_http_client",
  "hb_http_client_sup",
  "hb_http_multi",
  "hb_http_server",
  "hb_json",
  "hb_keccak",
  "hb_link",
  "hb_logger",
  "hb_maps",
  "hb_message",
  "hb_message_test_vectors",
  "hb_metrics_collector",
  "hb_name",
  "hb_opts",
  "hb_path",
  "hb_persistent",
  "hb_private",
  "hb_process_monitor",
  "hb_router",
  "hb_singleton",
  "hb_store",
  "hb_store_fs",
  "hb_store_gateway",
  "hb_store_lmdb",
  "hb_store_lru",
  "hb_store_opts",
  "hb_store_remote_node",
  "hb_store_rocksdb",
  "hb_structured_fields",
  "hb_sup",
  "hb_test_utils",
  "hb_tracer",
  "hb_util",
  "hb_volume",
]
const dev = [
  "dev_apply",
  "dev_arweave",
  "dev_arweave_block_cache",
  "dev_auth_hook",
  "dev_cache",
  "dev_cacheviz",
  "dev_codec_ans104",
  "dev_codec_ans104_from",
  "dev_codec_ans104_to",
  "dev_codec_cookie",
  "dev_codec_cookie_auth",
  "dev_codec_cookie_test_vectors",
  "dev_codec_flat",
  "dev_codec_http_auth",
  "dev_codec_httpsig",
  "dev_codec_httpsig_conv",
  "dev_codec_httpsig_keyid",
  "dev_codec_httpsig_proxy",
  "dev_codec_httpsig_siginfo",
  "dev_codec_json",
  "dev_codec_structured",
  "dev_copycat",
  "dev_copycat_arweave",
  "dev_copycat_graphql",
  "dev_cron",
  "dev_cu",
  "dev_dedup",
  "dev_delegated_compute",
  "dev_faff",
  "dev_genesis_wasm",
  "dev_green_zone",
  "dev_hook",
  "dev_hyperbuddy",
  "dev_json_iface",
  "dev_local_name",
  "dev_lookup",
  "dev_lua",
  "dev_lua_lib",
  "dev_lua_test",
  "dev_lua_test_ledgers",
  "dev_manifest",
  "dev_message",
  "dev_meta",
  "dev_monitor",
  "dev_multipass",
  "dev_name",
  "dev_node_process",
  "dev_p4",
  "dev_patch",
  "dev_poda",
  "dev_process",
  "dev_process_cache",
  "dev_process_worker",
  "dev_profile",
  "dev_push",
  "dev_query",
  "dev_query_arweave",
  "dev_query_graphql",
  "dev_query_test_vectors",
  "dev_relay",
  "dev_router",
  "dev_scheduler",
  "dev_scheduler_cache",
  "dev_scheduler_formats",
  "dev_scheduler_registry",
  "dev_scheduler_server",
  "dev_secret",
  "dev_simple_pay",
  "dev_snp",
  "dev_snp_nif",
  "dev_stack",
  "dev_test",
  "dev_trie",
  "dev_volume",
  "dev_wasi",
  "dev_wasm",
  "dev_whois",
]

export default defineConfig({
  iconUrl: "/favicon.ico",
  title: "WizardAO",
  ogImageUrl:
    "https://vocs.dev/api/og?logo=%logo&title=%title&description=%description",
  topNav: [
    { text: "Get Started", link: "/getting-started" },
    { text: "HyperBEAM", link: "/book" },
    { text: "Mobile", link: "/mobile" },
    { text: "SDK", link: "/api/ao" },
  ],
  socials: [
    {
      icon: "discord",
      link: "https://discord.gg/jGqWtUUntQ",
    },
    {
      icon: "github",
      link: "https://github.com/arweaveoasis/wao",
    },
    {
      icon: "x",
      link: "https://twitter.com/WaoEco",
    },
  ],
  sidebar: [
    {
      text: "Getting Started",
      link: "/getting-started",
    },
    {
      text: "Legacynet AOS",
      link: "/legacynet",
    },
    {
      text: "HyperBEAM",
      link: "/hb",
    },
    {
      text: "Decoding HyperBEAM",
      collapsed: true,
      items: [
        {
          text: "Overview",
          link: "/hyperbeam/decoding-from-scratch",
        },
        {
          text: "Installing HB and WAO",
          link: "/hyperbeam/installing-hb-wao",
        },
        {
          text: "Devices and Pathing",
          link: "/hyperbeam/devices-pathing",
        },
        {
          text: "Custom Devices and Codecs",
          link: "/hyperbeam/custom-devices-codecs",
        },
        {
          text: "Flat Codec",
          link: "/hyperbeam/codec-flat",
        },
        {
          text: "Structured Codec",
          link: "/hyperbeam/codec-structured",
        },
        {
          text: "Httpsig Codec",
          link: "/hyperbeam/codec-httpsig",
        },
        {
          text: "Http Message Signatures",
          link: "/hyperbeam/http-message-signatures",
        },
        {
          text: "Hashpaths",
          link: "/hyperbeam/hashpaths",
        },
        {
          text: "Device Composition",
          link: "/hyperbeam/device-composition",
        },
        {
          text: "Processes and Scheduler",
          link: "/hyperbeam/processes-scheduler",
        },
        {
          text: "Legacynet Compatible AOS",
          link: "/hyperbeam/legacynet-aos",
        },
        {
          text: "Payment System",
          link: "/hyperbeam/payment-system",
        },
      ],
    },
    {
      text: "Building Devices",
      collapsed: true,
      items: [
        {
          text: "Overview",
          link: "/book",
        },
        /*{
          text: "HyperBEAM",
          link: "/book/hyperbeam",
        },*/
        {
          text: "Setup",
          link: "/book/setup",
        },
        {
          text: "Erlang",
          link: "/book/erlang",
        },
        {
          text: "Structure",
          link: "/book/structure",
        },
        {
          text: "Arweave Utils",
          link: "/book/ar",
        },
        {
          text: "HB Core",
          collapsed: true,
          items: [
            {
              text: "Utilities",
              link: "/book/hb1",
            },
            {
              text: "Configuration",
              link: "/book/hb2",
            },
            {
              text: "Data Structure",
              link: "/book/hb3",
            },
            {
              text: "Storage",
              link: "/book/hb4",
            },
            {
              text: "Caching",
              link: "/book/hb5",
            },
            {
              text: "HTTP Client",
              link: "/book/hb6",
            },
            {
              text: "Remote Storage",
              link: "/book/hb7",
            },
            {
              text: "Protocol",
              link: "/book/hb8",
            },
            {
              text: "HTTP Server",
              link: "/book/hb9",
            },
            {
              text: "WASM",
              link: "/book/hb10",
            },
            {
              text: "Application",
              link: "/book/hb11",
            },
          ],
        },
      ],
    },
    {
      text: "HyperBEAM on Mobile",
      collapsed: true,
      items: [
        {
          text: "Overview",
          link: "/mobile",
        },
        {
          text: "Android",
          link: "/mobile/android",
        },
        {
          text: "iOS",
          link: "/mobile/ios",
        },
      ],
    },
    {
      text: "AO The Web",
      collapsed: true,
      items: [
        {
          text: "Overview",
          link: "/web",
        },
        {
          text: "WAO Hub",
          link: "/hub",
        },
      ],
    },
    {
      text: "Tutorials",
      collapsed: true,
      items: [
        {
          text: "Legacynet AOS on HyperBEAM",
          link: "/tutorials/legacynet-aos",
        },
        {
          text: "Mainnet AOS",
          link: "/tutorials/mainnet-aos",
        },
        {
          text: "Running LLMs on AOS",
          link: "/tutorials/running-llms",
        },
        {
          text: "Custom Devices in Erlang",
          link: "/tutorials/creating-devices",
        },
        {
          text: "Custom Devices in Rust",
          link: "/tutorials/devices-rust",
        },
        {
          text: "Custom Devices in C++",
          link: "/tutorials/devices-cpp",
        },
      ],
    },
    {
      text: "WAO SDK",
      collapsed: true,
      items: [
        {
          text: "AO",
          link: "/api/ao",
        },
        {
          text: "Process",
          link: "/api/process",
        },
        {
          text: "Function Piping",
          link: "/api/function-piping",
        },
        {
          text: "AR",
          link: "/api/ar",
        },
        {
          text: "GQL",
          link: "/api/gql",
        },
        {
          text: "ArMem",
          link: "/api/armem",
        },
        {
          text: "HB",
          link: "/api/hb",
        },
        {
          text: "HyperBEAM",
          link: "/api/hyperbeam",
        },
        {
          text: "HBSig",
          link: "/api/hbsig",
        },
      ],
    },
    {
      text: "HyperBEAM Reference",
      collapsed: true,
      items: [
        {
          text: "Overview",
          link: "/hyperbeam",
        },
        {
          text: "AR Utils",
          collapsed: true,
          items: [...ar.map(v => ({ text: v, link: `/src/${v}` }))],
        },
        {
          text: "HB Core",
          collapsed: true,
          items: [...hb.map(v => ({ text: v, link: `/src/${v}` }))],
        },
        {
          text: "Devices",
          collapsed: true,
          items: [...dev.map(v => ({ text: v, link: `/src/${v}` }))],
        },
      ],
    },
  ],
})
