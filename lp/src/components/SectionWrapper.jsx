import { useReveal } from '../hooks/useReveal'

export default function SectionWrapper({ id, className = '', stagger = false, children }) {
  const ref = useReveal(0.1)

  return (
    <section
      id={id}
      ref={ref}
      className={`${stagger ? 'reveal-stagger' : 'reveal'} ${className}`}
    >
      {children}
    </section>
  )
}
