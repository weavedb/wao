import { useState } from 'react'
import s from './CodeBlock.module.css'

export default function CodeBlock({ tabs }) {
  const [active, setActive] = useState(0)
  const [copied, setCopied] = useState(false)

  const copy = () => {
    const text = tabs[active].raw || tabs[active].code
    navigator.clipboard.writeText(text).then(() => {
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    })
  }

  return (
    <div className={s.wrapper}>
      <div className={s.header}>
        <div className={s.tabs}>
          {tabs.map((tab, i) => (
            <button
              key={tab.label}
              className={`${s.tab} ${i === active ? s.tabActive : ''}`}
              onClick={() => setActive(i)}
            >
              {tab.label}
            </button>
          ))}
        </div>
        <button className={s.copyBtn} onClick={copy}>
          {copied ? 'Copied!' : 'Copy'}
        </button>
      </div>
      <div className={s.code} dangerouslySetInnerHTML={{ __html: tabs[active].code }} />
    </div>
  )
}
