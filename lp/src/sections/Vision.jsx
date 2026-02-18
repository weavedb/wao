import SectionWrapper from '../components/SectionWrapper'
import { scrollTo } from '../utils/scrollTo'
import s from './Vision.module.css'

const STEPS = [
  {
    num: '01',
    label: 'Learn',
    title: 'The knowledge base',
    desc: 'The most comprehensive AO reference — 139 modules across 7 chapters, built for devs and agents alike.',
    id: 'book',
    color: '#3b82f6',
  },
  {
    num: '02',
    label: 'Code',
    title: 'Write less code',
    desc: '3 lines instead of 30+. A radically simplified SDK for deploying and messaging on AO.',
    id: 'sdk',
    color: '#a855f7',
  },
  {
    num: '03',
    label: 'Test',
    title: 'Lightning fast testing',
    desc: '5 environments from sub-second in-memory to full remote — one API, zero code changes.',
    id: 'testing',
    color: '#f43f5e',
  },
  {
    num: '04',
    label: 'Build with AI',
    title: 'Write no code',
    desc: 'AI agents that scaffold, build, test, and ship your AO apps — with persistent context and quality gates.',
    id: 'hyperadd',
    color: '#f59e0b',
  },
  {
    num: '05',
    label: 'Deploy',
    title: 'Full AO on the edge',
    desc: 'All 5 AO/Arweave units in one Cloudflare Worker. Free tier, instant reset.',
    id: 'devnet',
    color: '#22c55e',
  },
]

export default function Vision() {
  const handleClick = (e, id) => {
    e.preventDefault()
    scrollTo(id)
  }

  return (
    <SectionWrapper id="journey" className={s.section}>
      <div className={s.inner}>
        <div className={s.header}>
          <span className="eyebrow">Your Journey</span>
          <h2 className={s.title}>
            From first line to<br />
            <span className={s.titleGradient}>production app.</span>
          </h2>
          <p className="section-subtitle" style={{ margin: '0 auto' }}>
            Everything you need to go from learning AO and HyperBEAM
            to shipping verifiable applications — in one toolkit.
          </p>
        </div>

        <div className={s.path}>
          {STEPS.map((step) => (
            <a
              href={`#${step.id}`}
              className={s.step}
              key={step.num}
              style={{ '--step-color': step.color }}
              onClick={e => handleClick(e, step.id)}
            >
              <div className={s.stepLeft}>
                <span className={s.stepNum}>{step.num}</span>
                <div className={s.stepLine} />
              </div>
              <div className={s.stepContent}>
                <span className={s.stepLabel}>{step.label}</span>
                <h3 className={s.stepTitle}>{step.title}</h3>
                <p className={s.stepDesc}>{step.desc}</p>
              </div>
            </a>
          ))}
        </div>
      </div>
    </SectionWrapper>
  )
}
