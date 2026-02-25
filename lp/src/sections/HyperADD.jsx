import SectionWrapper from '../components/SectionWrapper'
import s from './HyperADD.module.css'

const FEATURES = [
  'Scaffold projects instantly',
  '10 specialized HyperWizard roles',
  'Quality gates & enforcing hooks',
  '20+ slash commands',
  'Persistent task tracking',
  'Structured agent handoffs',
]

export default function HyperADD() {
  return (
    <SectionWrapper id="hyperadd" className={s.section}>
      {/* Full-bleed background image */}
      <div className={s.bgImage} />
      <div className={s.bgOverlay} />

      <div className={s.heroContent}>
        <div className={s.textCol}>
          <span className={`eyebrow ${s.eyebrow}`}>Step 04 &middot; Build with AI</span>
          <h2 className={`section-title ${s.title}`}>Write no code.</h2>
          <p className={s.subtitle}>
            AI agents that scaffold, build, test, and ship your AO applications.
            Persistent context, quality gates, and structured handoffs — you direct, they deliver.
          </p>

          <div className={s.pills}>
            {FEATURES.map(f => (
              <span className={s.pill} key={f}>{f}</span>
            ))}
          </div>

          <a href="https://docs.wao.eco/add/overview" className={`btn-primary ${s.ctaBtn}`} target="_blank" rel="noopener noreferrer">
            Build with HyperADD
          </a>
        </div>

        <div className={s.screenshotCol}>
          <img src="/images/dashboard.png" alt="HyperADD Dashboard" className={s.screenshot} />
        </div>
      </div>
    </SectionWrapper>
  )
}
