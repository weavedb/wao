import SectionWrapper from '../components/SectionWrapper'
import s from './Devnet.module.css'

export default function Devnet() {
  return (
    <SectionWrapper id="devnet" className={s.section}>
      {/* Full-bleed background image */}
      <div className={s.bgImage} />
      <div className={s.bgOverlay} />

      <div className={s.heroContent}>
        <div className={s.textCol}>
          <span className={`eyebrow ${s.eyebrow}`}>Step 05 &middot; Deploy</span>
          <h2 className={`section-title ${s.title}`}>Full AO on the edge.</h2>
          <p className={s.subtitle}>
            A complete AO development environment running on Cloudflare Workers.
            All 5 AO/Arweave units in one Worker — deploy, test, and iterate instantly.
          </p>

          <div className={s.features}>
            <div className={s.feature}>
              <span className={s.featureDot} />
              <div>
                <span className={s.featureTitle}>All 5 AO/Arweave units in one Worker</span>
                <span className={s.featureDesc}>MU, SU, CU, Scheduler, and Process in a single deployment.</span>
              </div>
            </div>
            <div className={s.feature}>
              <span className={s.featureDot} />
              <div>
                <span className={s.featureTitle}>WizardAO Scan explorer</span>
                <span className={s.featureDesc}>Built-in block explorer for inspecting messages, processes, and state.</span>
              </div>
            </div>
            <div className={s.feature}>
              <span className={s.featureDot} />
              <div>
                <span className={s.featureTitle}>Free tier &amp; instant reset</span>
                <span className={s.featureDesc}>Zero cost to start. Reset state anytime during development.</span>
              </div>
            </div>
          </div>

          <a href="https://docs.wao.eco/devnet/overview" className={`btn-primary ${s.ctaBtn}`} target="_blank" rel="noopener noreferrer">
            Try WAO DEVNET
          </a>
        </div>

        <div className={s.screenshotCol}>
          <img src="/images/scan.png" alt="WizardAO Scan Explorer" className={s.screenshot} />
        </div>
      </div>
    </SectionWrapper>
  )
}
