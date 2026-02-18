import { useEffect, useRef } from 'react'
import { scrollTo } from '../utils/scrollTo'
import s from './Hero.module.css'

const STATS = [
  { value: '5', label: 'Test Envs' },
  { value: '1000x', label: 'Faster' },
  { value: 'Free', label: 'To Start' },
]

export default function Hero() {
  const sectionRef = useRef(null)

  useEffect(() => {
    const el = sectionRef.current
    if (!el) return
    requestAnimationFrame(() => el.classList.add(s.entered))
  }, [])

  return (
    <section className={s.hero} id="hero" ref={sectionRef}>
      <div className={s.meshBg} />

      <div className={s.content}>
        <span className={s.badge}>AO &amp; HyperBEAM Toolchain</span>
        <h1 className={s.title}>
          Agent Driven Development<br />
          for the <span className={s.gradient}>Verifiable Internet.</span>
        </h1>

        <p className={s.subtitle}>
          Write less code with the SDK. Write no code with AI agents.
          Test at lightning speed. Learn from the most comprehensive
          AO knowledge base — built for devs and agents alike.
        </p>

        <div className={s.ctas}>
          <a
            href="#get-started"
            className="btn-primary"
            onClick={e => { e.preventDefault(); scrollTo('get-started') }}
          >
            Start Your Journey
          </a>
          <a
            href="https://github.com/arweaveoasis/wao"
            className="btn-ghost"
            target="_blank"
            rel="noopener noreferrer"
          >
            View on GitHub
          </a>
        </div>

        <div className={s.stats}>
          {STATS.map(st => (
            <div className={s.stat} key={st.label}>
              <span className={s.statValue}>{st.value}</span>
              <span className={s.statLabel}>{st.label}</span>
            </div>
          ))}
        </div>
      </div>

    </section>
  )
}
