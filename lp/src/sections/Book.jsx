import SectionWrapper from '../components/SectionWrapper'
import s from './Book.module.css'

const CHAPTERS = [
  { ch: 'Ch 0', title: 'HyperBEAM', desc: 'The OS for the verifiable internet.', count: 'Permaweb' },
  { ch: 'Ch 1', title: 'Setup', desc: 'Environment setup and getting started.', count: 'Environment' },
  { ch: 'Ch 2', title: 'Erlang', desc: 'Enough Erlang to read HyperBEAM source.', count: 'Crash course' },
  { ch: 'Ch 3', title: 'Project Structure', desc: 'How the HyperBEAM codebase is organized.', count: 'Codebase' },
  { ch: 'Ch 4', title: 'Arweave Utils', desc: 'Transaction helpers, wallets, and GraphQL.', count: '7 modules' },
  { ch: 'Ch 5', title: 'HyperBEAM Core', desc: 'Runtime, routing, scheduling, and message passing.', count: '11 sections' },
  { ch: 'Ch 6', title: 'Devices', desc: 'Compute, storage, cron, payment, and auth devices.', count: '9 sections' },
  { ch: 'Ch 7', title: 'Building Your Own Devices', desc: 'From beginner to advanced — Erlang, Rust, and C++.', count: '5 tutorials' },
]

export default function Book() {
  return (
    <SectionWrapper id="book" className={s.section}>
      <div className={s.inner}>
        <div className={s.imageCol}>
          <span className={`eyebrow ${s.eyebrow}`}>Step 01 &middot; Learn</span>
          <h2 className="section-title">The knowledge base<br />for devs and agents.</h2>
          <p className={`section-subtitle ${s.subtitle}`}>
            7 chapters covering AO and HyperBEAM from the ground up, plus a
            comprehensive 139-module reference — for devs and AI agents alike.
          </p>
          <div className={s.imageWrap}>
            <div className={s.glow} />
            <div className={s.book}>
              <img src="/images/book.jpg" alt="HyperBEAM Book" className={s.bookCover} />
            </div>
          </div>
          <div className={s.btnRow}>
            <a href="https://docs.wao.arweaveoasis.com/book" className={`btn-primary ${s.ctaBtn}`} target="_blank" rel="noopener noreferrer">
              Read the Book
            </a>
            <a href="https://docs.wao.arweaveoasis.com/hyperbeam" className={`btn-ghost ${s.ctaBtn}`} target="_blank" rel="noopener noreferrer">
              Go to Reference
            </a>
          </div>
        </div>

        <div className={s.textCol}>
          <div className={s.tocPill}>Table of Contents</div>
          <div className={s.chapters}>
            {CHAPTERS.map(ch => (
              <div className={s.chapter} key={ch.ch}>
                <span className={s.chapterNum}>{ch.ch}</span>
                <div className={s.chapterContent}>
                  <span className={s.chapterTitle}>{ch.title}</span>
                  <span className={s.chapterDesc}>{ch.desc}</span>
                </div>
                {ch.count && <span className={s.chapterCount}>{ch.count}</span>}
              </div>
            ))}
          </div>
        </div>
      </div>
    </SectionWrapper>
  )
}
