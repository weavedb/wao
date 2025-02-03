import { Link, ssr } from "arnext"
import { useEffect, useState } from "react"
import wasm from "../lib/wasm"
import { AO } from "../../src/web"
//import { AO } from "wao/web"
import client from "../lib/client"
const getDate = async date => date ?? Date.now()

export const getStaticProps = ssr(async ({}) => {
  return { props: { _date: Date.now() }, revalidate: 100 }
})

const genMsg = async (Id, p, data, Tags, from, Owner, dry = false) => {
  if (!dry) p.height += 1
  return {
    Id,
    Target: p.id,
    Owner,
    Data: data?.length ? data : "",
    "Block-Height": "1",
    Timestamp: Date.now().toString(),
    Module: p.module,
    From: from,
    Cron: false,
    Tags: Tags?.length ? Tags : [],
  }
}

const genEnv = async ({ pid, owner = "", module = "" }) => {
  return {
    Process: {
      Id: pid,
      Tags: [],
      Owner: owner,
    },
    Module: {
      Id: module,
      Tags: [],
    },
  }
}

export default function Home({ _date = null }) {
  const [date, setDate] = useState(_date)
  useEffect(() => {}, [])
  return (
    <>
      <div
        onClick={async () => {
          const _client = Buffer.from(client, "base64").toString()

          const ao = new AO()
          const { p, pid } = await ao.deploy({ src_data: _client })
          console.log(
            await p.m("Set", {
              Query: JSON.stringify([{ name: "Bob" }, "ppl", "Bob"]),
            }),
          )
          console.log(await p.d("Get", { Query: JSON.stringify(["ppl"]) }))
        }}
      >
        Deploy WeaveDB Lite
      </div>
    </>
  )
}
