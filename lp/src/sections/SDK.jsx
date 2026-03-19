import SectionWrapper from '../components/SectionWrapper'
import CodeBlock from '../components/CodeBlock'
import s from './SDK.module.css'

const WAO_CODE = `<span class="kw">import</span> { <span class="fn">AO</span>, <span class="fn">acc</span> } <span class="kw">from</span> <span class="str">"wao/test"</span>

<span class="kw">const</span> ao = <span class="kw">await new</span> <span class="fn">AO</span>().<span class="fn">init</span>(<span class="fn">acc</span>[0])
<span class="kw">const</span> { p } = <span class="kw">await</span> ao.<span class="fn">deploy</span>({ src_data })
assert.<span class="fn">equal</span>(<span class="kw">await</span> p.<span class="fn">d</span>(<span class="str">"Hello"</span>, false), <span class="str">"Hello, World!"</span>)`

const TRAD_CODE = `<span class="kw">import</span> { <span class="fn">connect</span>, <span class="fn">acc</span> } <span class="kw">from</span> <span class="str">"wao/test"</span>

<span class="kw">const</span> { spawn, message, dryrun } = <span class="fn">connect</span>()
<span class="kw">const</span> signer = <span class="fn">acc</span>[0].signer

<span class="kw">const</span> pid = <span class="kw">await</span> <span class="fn">spawn</span>({
  signer,
  module: <span class="str">"Do_Uc2Sju_ffp6Ev0AnLVdPtot15rvMjP..."</span>,
  scheduler: <span class="str">"_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu..."</span>
})

<span class="kw">await</span> <span class="fn">message</span>({
  process: pid,
  tags: [{ name: <span class="str">"Action"</span>, value: <span class="str">"Eval"</span> }],
  data: src_data, signer,
})

<span class="kw">const</span> res = <span class="kw">await</span> <span class="fn">dryrun</span>({
  process: pid,
  tags: [{ name: <span class="str">"Action"</span>, value: <span class="str">"Hello"</span> }],
  signer,
})
assert.<span class="fn">equal</span>(res.Messages[0].Data, <span class="str">"Hello, World!"</span>)`

const FEATURES = [
  'Concise, intuitive API — 3 lines instead of 30+',
  'Built-in check/get validation pattern',
  'Function piping with pipe() for composable flows',
  'Unified modules: AO, AR, HB, GQL, and HBSig',
]

export default function SDK() {
  return (
    <SectionWrapper id="sdk" className={s.section}>
      <div className={s.separator} />
      <div className={s.inner}>
        <div className={s.text}>
          <span className="eyebrow">Step 02 &middot; Code</span>
          <h2 className="section-title">Write less code.</h2>
          <p className="section-subtitle">
            3 lines instead of 30+. A radically simplified developer experience
            for Arweave and AO — everything you need, nothing you don't.
          </p>
          <div className={s.features}>
            {FEATURES.map(f => (
              <div className={s.feature} key={f}>
                <span className={s.featureDot} />
                <span>{f}</span>
              </div>
            ))}
          </div>

          <a href="https://docs.wao.arweaveoasis.com/api/overview" className="btn-primary" style={{ marginTop: '24px', width: 'fit-content' }} target="_blank" rel="noopener noreferrer">
            Explore the SDK
          </a>
        </div>

        <div className={s.codeCol}>
          <div className={s.linesPill}>3 lines vs 15+</div>
          <div className={s.codeWrap}>
            <CodeBlock
              tabs={[
                {
                  label: 'WAO SDK',
                  code: WAO_CODE,
                  raw: `import { AO, acc } from "wao/test"\n\nconst ao = await new AO().init(acc[0])\nconst { p } = await ao.deploy({ src_data })\nassert.equal(await p.d("Hello", false), "Hello, World!")`,
                },
                {
                  label: 'Traditional',
                  code: TRAD_CODE,
                  raw: `import { connect, acc } from "wao/test"\n\nconst { spawn, message, dryrun } = connect()\nconst signer = acc[0].signer\n\nconst pid = await spawn({ signer, module: "...", scheduler: "..." })\nawait message({ process: pid, tags: [...], data: src_data, signer })\nconst res = await dryrun({ process: pid, tags: [...], signer })\nassert.equal(res.Messages[0].Data, "Hello, World!")`,
                },
              ]}
            />
          </div>
        </div>
      </div>
    </SectionWrapper>
  )
}
