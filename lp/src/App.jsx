import Nav from './components/Nav'
import Footer from './components/Footer'
import Hero from './sections/Hero'
import Vision from './sections/Vision'
import Book from './sections/Book'
import SDK from './sections/SDK'
import Testing from './sections/Testing'
import HyperADD from './sections/HyperADD'
import Devnet from './sections/Devnet'
import GetStarted from './sections/GetStarted'

export default function App() {
  return (
    <>
      <Nav />
      <main>
        <Hero />
        <Vision />
        <Book />
        <SDK />
        <Testing />
        <HyperADD />
        <Devnet />
        <GetStarted />
      </main>
      <Footer />
    </>
  )
}
