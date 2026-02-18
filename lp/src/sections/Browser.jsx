import SectionWrapper from '../components/SectionWrapper'
import s from './Browser.module.css'

const FEATURES = [
  'wao/web SDK',
  'ArConnect wallet',
  'WebRTC mesh',
  'Zero server',
]

export default function Browser() {
  return (
    <SectionWrapper id="browser" className={s.section}>
      <div className={s.inner}>
        <div className={s.textCol}>
          <span className="eyebrow">Step 06 &middot; Ship</span>
          <h2 className="section-title">Ship it. Everywhere.</h2>
          <p className="section-subtitle">
            Full AO compute running client-side in the browser.
            No servers, no backends — just a wallet and a web page.
          </p>

          <div className={s.features}>
            {FEATURES.map(f => (
              <span className={s.featureItem} key={f}>
                <span className={s.featureDot} /> {f}
              </span>
            ))}
          </div>
        </div>

        <div className={s.screenshotCol}>
          <div className={`browser-chrome ${s.browserFrame}`}>
            <div className="browser-chrome-bar">
              <span className="browser-chrome-dot" />
              <span className="browser-chrome-dot" />
              <span className="browser-chrome-dot" />
              <span className="browser-chrome-url">preview.wao.eco</span>
            </div>
            <img src="/images/wao-web.png" alt="WizardAO in the Browser" />
          </div>
        </div>
      </div>
    </SectionWrapper>
  )
}
