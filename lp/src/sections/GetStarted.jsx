import { useState } from 'react'
import SectionWrapper from '../components/SectionWrapper'
import s from './GetStarted.module.css'

const CMD = 'npx wao create myapp'

export default function GetStarted() {
  const [copied, setCopied] = useState(false)

  const copy = () => {
    navigator.clipboard.writeText(CMD).then(() => {
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    })
  }

  return (
    <SectionWrapper id="get-started" className={s.section}>
      <div className={s.inner}>
        <span className={s.eyebrow}>Your journey starts now</span>
        <h2 className={s.title}>
          One command.<br />
          The entire verifiable stack.
        </h2>
        <p className={s.desc}>
          Less code with the SDK. No code with agents. Lightning fast testing.
          A knowledge base for humans and AI. All in one command.
        </p>

        <div className={s.codeWrap}>
          <span className={s.code}>
            <span className={s.prompt}>$</span>{' '}
            <span className={s.cmd}>{CMD}</span>
          </span>
          <button className={s.copyBtn} onClick={copy}>
            {copied ? 'Copied!' : 'Copy'}
          </button>
        </div>

        <div className={s.ctas}>
          <a href="https://docs.wao.eco" className="btn-primary" target="_blank" rel="noopener noreferrer">
            Read the Docs
          </a>
          <a href="https://github.com/arweaveoasis/wao" className="btn-ghost" target="_blank" rel="noopener noreferrer">
            View on GitHub
          </a>
        </div>
      </div>
    </SectionWrapper>
  )
}
