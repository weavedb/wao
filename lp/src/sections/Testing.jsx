import SectionWrapper from '../components/SectionWrapper'
import s from './Testing.module.css'

const ENVS = [
  { num: '01', title: 'In-Memory AOS', speed: 'Lightning Fast', color: '#34d399' },
  { num: '02', title: 'Local AO Units', speed: 'Your Machine', color: '#60a5fa' },
  { num: '03', title: 'Local HyperBEAM', speed: 'Sandboxed', color: '#a78bfa' },
  { num: '04', title: 'WAO DEVNET', speed: 'Free & Scalable', color: '#f43f5e' },
  { num: '05', title: 'Remote HyperBEAM', speed: 'Production', color: '#f59e0b' },
]

export default function Testing() {
  return (
    <SectionWrapper id="testing" className={s.section}>
      <div className={s.inner}>
        <div className={s.header}>
          <span className="eyebrow">Step 03 &middot; Test</span>
          <h2 className="section-title">Lightning fast testing.</h2>
          <p className="section-subtitle" style={{ margin: '0 auto' }}>
            Five environments from sub-second in-memory to full remote —
            one API, zero code changes. Test at the speed you think, or let wizard agents handle.
          </p>
        </div>

        <div className={s.spectrum}>
          <span className={s.specLabel}>Fast</span>
          <span className={s.specLabel}>Realistic</span>
        </div>
        <div className={s.specBar} />

        <div className={s.cards}>
          {ENVS.map((env) => (
            <div
              className={s.card}
              key={env.num}
              style={{ '--card-color': env.color }}
            >
              <span className={s.cardNum}>{env.num}</span>
              <span className={s.cardTitle}>{env.title}</span>
              <span className={s.cardSpeed}>{env.speed}</span>
            </div>
          ))}
        </div>
      </div>
    </SectionWrapper>
  )
}
