import { useState } from 'react'
import { useScrolled } from '../hooks/useScrolled'
import { scrollTo } from '../utils/scrollTo'
import s from './Nav.module.css'

const NAV_LINKS = [
  { label: 'Journey', id: 'journey' },
  { label: 'Book', id: 'book' },
  { label: 'SDK', id: 'sdk' },
  { label: 'Testing', id: 'testing' },
  { label: 'HyperADD', id: 'hyperadd' },
  { label: 'DEVNET', id: 'devnet' },
]

export default function Nav() {
  const scrolled = useScrolled(80)
  const [open, setOpen] = useState(false)

  const closeMenu = () => setOpen(false)

  const handleNav = (e, id) => {
    e.preventDefault()
    closeMenu()
    scrollTo(id)
  }

  return (
    <>
      <nav className={`${s.nav} ${scrolled ? s.scrolled : ''}`}>
        <div className={s.inner}>
          <a
            href="/"
            className={s.logo}
            onClick={e => { e.preventDefault(); window.scrollTo({ top: 0, behavior: 'smooth' }) }}
          >
            <img src="/logo.png" alt="" className={s.logoIcon} />
            WizardAO
          </a>

          <div className={s.links}>
            {NAV_LINKS.map(l => (
              <a key={l.id} href={`#${l.id}`} className={s.link} onClick={e => handleNav(e, l.id)}>
                {l.label}
              </a>
            ))}
          </div>

          <div className={s.ctas}>
            <a href="https://docs.wao.eco" className={s.docsBtn} target="_blank" rel="noopener noreferrer">Docs</a>
            <a href="#get-started" className={s.startBtn} onClick={e => handleNav(e, 'get-started')}>
              Get Started
            </a>
          </div>

          <button
            className={`${s.hamburger} ${open ? s.hamburgerOpen : ''}`}
            onClick={() => setOpen(!open)}
            aria-label="Menu"
          >
            <span /><span /><span />
          </button>
        </div>
      </nav>

      <div className={`${s.mobileMenu} ${open ? s.mobileMenuOpen : ''}`}>
        {NAV_LINKS.map(l => (
          <a key={l.id} href={`#${l.id}`} className={s.mobileLink} onClick={e => handleNav(e, l.id)}>
            {l.label}
          </a>
        ))}
        <div className={s.mobileCtas}>
          <a href="https://docs.wao.eco" className={s.docsBtn} target="_blank" rel="noopener noreferrer" onClick={closeMenu}>Docs</a>
          <a href="#get-started" className={s.startBtn} onClick={e => handleNav(e, 'get-started')}>
            Get Started
          </a>
        </div>
      </div>
    </>
  )
}
