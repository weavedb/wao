import { useState, useEffect, useRef } from "react"
import Head from "next/head"

export default function Home() {
  // Set the launch date (modify this to your actual launch date)
  const launchDate = new Date("2025-04-27T00:00:00")

  const [timeLeft, setTimeLeft] = useState({
    days: 0,
    hours: 0,
    minutes: 0,
    seconds: 0,
  })

  // Matrix animation canvas reference
  const canvasRef = useRef(null)

  useEffect(() => {
    const calculateTimeLeft = () => {
      const difference = launchDate - new Date()

      if (difference > 0) {
        setTimeLeft({
          days: Math.floor(difference / (1000 * 60 * 60 * 24)),
          hours: Math.floor((difference / (1000 * 60 * 60)) % 24),
          minutes: Math.floor((difference / 1000 / 60) % 60),
          seconds: Math.floor((difference / 1000) % 60),
        })
      } else {
        setTimeLeft({ days: 0, hours: 0, minutes: 0, seconds: 0 })
      }
    }

    // Calculate immediately
    calculateTimeLeft()

    // Update every second
    const timerId = setInterval(calculateTimeLeft, 1000)

    // Clean up the interval on component unmount
    return () => clearInterval(timerId)
  }, [])

  // Matrix animation effect
  // Canvas scale factor to ensure matrix is visible at any size
  const SCALE_FACTOR = 0.5

  useEffect(() => {
    if (!canvasRef.current) return

    const canvas = canvasRef.current
    const ctx = canvas.getContext("2d")

    // Set canvas dimensions to match screen size with a scale factor for performance
    const resizeCanvas = () => {
      const width = window.innerWidth
      const height = window.innerHeight

      // Set the canvas size to the actual device pixel ratio to avoid blurriness
      const dpr = window.devicePixelRatio || 1
      canvas.width = width * dpr * SCALE_FACTOR
      canvas.height = height * dpr * SCALE_FACTOR

      // Scale the context to account for the device pixel ratio
      ctx.scale(dpr * SCALE_FACTOR, dpr * SCALE_FACTOR)

      // Set the CSS size of the canvas
      canvas.style.width = `${width}px`
      canvas.style.height = `${height}px`

      // When resizing, we need to recalculate the columns and drops
      initializeMatrix(width, height)
    }

    // Matrix character set - can modify for different looks
    const characters =
      "アァカサタナハマヤャラワガザダバパイィキシチニヒミリヰギジヂビピウゥクスツヌフムユュルグズブヅプエェケセテネヘメレヱゲゼデベペオォコソトノホモヨョロヲゴゾドボポヴッン0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    const charArray = characters.split("")

    // Adjust fontSize based on screen width
    const getFontSize = () => {
      if (window.innerWidth <= 360) return 8
      if (window.innerWidth <= 480) return 10
      if (window.innerWidth <= 768) return 12
      return 14
    }

    let fontSize = 14
    let columns = 0
    let drops = []

    // Initialize matrix parameters
    const initializeMatrix = (width, height) => {
      fontSize = getFontSize()
      columns = Math.floor(width / fontSize)

      // Reinitialize the drops array
      drops = []
      for (let i = 0; i < columns; i++) {
        drops[i] = Math.random() * -100 // Start above the screen at random positions
      }
    }

    // Initial setup
    resizeCanvas()
    window.addEventListener("resize", resizeCanvas)

    // Draw function
    const draw = () => {
      // Semi-transparent black to create the fade effect
      ctx.fillStyle = "rgba(25, 15, 53, 0.05)"
      ctx.fillRect(
        0,
        0,
        canvas.width / (SCALE_FACTOR * (window.devicePixelRatio || 1)),
        canvas.height / (SCALE_FACTOR * (window.devicePixelRatio || 1))
      )

      // Set the color and font of the characters
      ctx.fillStyle = "#c252ff" // Purple color
      ctx.font = `${fontSize}px monospace`

      // Loop over each drop
      for (let i = 0; i < drops.length; i++) {
        // Choose a random character
        const char = charArray[Math.floor(Math.random() * charArray.length)]

        // x coordinate of the drop (column * fontSize)
        const x = i * fontSize

        // y coordinate of the drop (current position)
        const y = drops[i] * fontSize

        // Draw the character
        ctx.fillText(char, x, y)

        // If the drop has reached the bottom or randomly (to create randomness in the flow)
        if (
          y > canvas.height / (SCALE_FACTOR * (window.devicePixelRatio || 1)) &&
          Math.random() > 0.975
        ) {
          drops[i] = 0 // Reset to the top
        }

        // Move the drop down by one position
        drops[i]++
      }
    }

    // Animation loop
    const animationId = setInterval(draw, 33) // ~30fps

    // Cleanup
    return () => {
      clearInterval(animationId)
      window.removeEventListener("resize", resizeCanvas)
    }
  }, [])

  return (
    <>
      <Head>
        <title>WAO | DevNet Launch</title>
        <meta name="description" content="Countdown to our WAO DevNet launch" />
        <meta
          name="viewport"
          content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
        />
        <link rel="icon" href="/favicon.ico" />
        <link
          href="https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700&family=Roboto:wght@300;400;700&display=swap"
          rel="stylesheet"
        />
      </Head>

      <style jsx global>{`
        * {
          box-sizing: border-box;
          margin: 0;
          padding: 0;
        }

        html,
        body {
          font-family: "Roboto", sans-serif;
          background-color: #190f35;
          color: white;
          overflow-x: hidden;
          width: 100%;
          height: 100%;
        }

        a {
          color: inherit;
          text-decoration: none;
        }

        .desktop-nav {
          display: flex;
          gap: 2rem;
        }

        @media (max-width: 768px) {
          .desktop-nav {
            display: none;
          }

          .mobile-menu-icon {
            display: block !important;
          }

          .main-title {
            font-size: 2.25rem !important;
          }

          .main-description {
            font-size: 1rem !important;
            margin: 0 auto 2rem !important;
          }

          .countdown-grid {
            grid-template-columns: repeat(2, 1fr) !important;
            gap: 1rem !important;
            margin: 0 auto 2rem !important;
          }

          .countdown-number {
            font-size: 2.75rem !important;
          }

          .footer-content {
            flex-direction: column !important;
            align-items: center !important;
            text-align: center !important;
          }
        }

        @media (max-width: 480px) {
          .main-title {
            font-size: 1.75rem !important;
          }

          .countdown-grid {
            gap: 0.75rem !important;
            padding: 0 0.5rem !important;
          }

          .countdown-number {
            font-size: 2.25rem !important;
          }
        }

        @media (max-width: 400px) {
          .main-title {
            font-size: 1.5rem !important;
            margin-bottom: 1rem !important;
          }

          .countdown-box {
            padding: 0.75rem 0.5rem !important;
            border-radius: 0.5rem !important;
          }

          .main-description {
            font-size: 0.85rem !important;
            margin: 0 auto 1.25rem !important;
            padding: 0 0.5rem !important;
            max-width: 280px !important;
          }

          .countdown-grid {
            display: flex !important;
            flex-direction: row !important;
            flex-wrap: wrap !important;
            grid-template-columns: unset !important;
            gap: 0.5rem !important;
            margin: 0 auto 1.5rem !important;
            max-width: 280px !important;
            padding: 0 !important;
            justify-content: center !important;
          }

          .countdown-box {
            width: 48% !important;
            flex: 0 0 auto !important;
          }

          .countdown-number {
            font-size: 1.5rem !important;
            margin-bottom: 0.25rem !important;
          }

          .countdown-label {
            font-size: 0.6rem !important;
            margin-top: 0.25rem !important;
          }

          .join-button {
            padding: 0.5rem 1.25rem !important;
            font-size: 0.85rem !important;
          }

          .header-content {
            padding: 0 0.5rem !important;
          }

          .logo {
            font-size: 1.5rem !important;
          }

          .footer-content {
            padding: 0 0.5rem !important;
          }

          .footer-text {
            font-size: 0.7rem !important;
          }

          .social-links {
            gap: 1rem !important;
          }

          .social-icon {
            width: 16px !important;
            height: 16px !important;
          }
        }

        @media (max-width: 360px) {
          .countdown-grid {
            flex-direction: column !important;
            max-width: 130px !important;
          }

          .countdown-box {
            width: 100% !important;
          }

          .main-title {
            font-size: 1.25rem !important;
          }

          .countdown-number {
            font-size: 1.25rem !important;
          }
        }
      `}</style>

      {/* Matrix Canvas */}
      <canvas
        ref={canvasRef}
        style={{
          position: "fixed",
          top: 0,
          left: 0,
          width: "100%",
          height: "100%",
          zIndex: 0,
          opacity: 0.7,
          pointerEvents: "none",
        }}
      />

      {/* Main Container */}
      <div
        style={{
          minHeight: "100vh",
          background:
            "linear-gradient(to bottom, rgba(25, 15, 53, 0.7), rgba(45, 27, 94, 0.7))",
          display: "flex",
          flexDirection: "column",
          position: "relative",
          zIndex: 1,
        }}
      >
        {/* Header */}
        <header
          style={{
            background:
              "linear-gradient(90deg, rgba(45, 27, 94, 0.9), rgba(121, 40, 202, 0.9))",
            padding: "1rem",
            borderBottom: "1px solid rgba(194, 82, 255, 0.3)",
            boxShadow: "0 0 15px rgba(194, 82, 255, 0.5)",
            backdropFilter: "blur(5px)",
            position: "relative",
            zIndex: 2,
          }}
        >
          <div
            className="header-content"
            style={{
              maxWidth: "1200px",
              margin: "0 auto",
              display: "flex",
              justifyContent: "space-between",
              alignItems: "center",
              padding: "0 1rem",
            }}
          >
            <h1
              className="logo"
              style={{
                fontFamily: "'Orbitron', sans-serif",
                fontSize: "2rem",
                fontWeight: "bold",
                color: "white",
                textShadow: "0 0 10px #c252ff",
              }}
            >
              WAO
            </h1>
            <nav
              className="desktop-nav"
              style={{ display: "flex", gap: "2rem" }}
            >
              <a
                href="https://docs.wao.eco"
                style={{ color: "#d8b4fe", fontWeight: "bold" }}
              >
                Docs
              </a>
            </nav>
            <div
              className="mobile-menu-icon"
              style={{
                display: "none",
              }}
            >
              {/* Mobile menu icon */}
              <svg width="24" height="24" viewBox="0 0 24 24" fill="#d8b4fe">
                <rect y="4" width="24" height="2" rx="1" />
                <rect y="11" width="24" height="2" rx="1" />
                <rect y="18" width="24" height="2" rx="1" />
              </svg>
            </div>
          </div>
        </header>

        {/* Main Content */}
        <main
          style={{
            flexGrow: 1,
            display: "flex",
            flexDirection: "column",
            justifyContent: "center",
            alignItems: "center",
            padding: "4rem 1rem",
            position: "relative",
            zIndex: 1,
          }}
        >
          <div
            style={{
              textAlign: "center",
              maxWidth: "1200px",
              margin: "0 auto",
              padding: "0 1rem",
            }}
          >
            <h2
              className="main-title"
              style={{
                fontFamily: "'Orbitron', sans-serif",
                fontSize: "3rem",
                marginBottom: "1.5rem",
                background:
                  "linear-gradient(to right, #d8b4fe, white, #d8b4fe)",
                WebkitBackgroundClip: "text",
                WebkitTextFillColor: "transparent",
                textShadow: "0 0 10px #c252ff",
              }}
            >
              WAO DevNet Launch
            </h2>
            <p
              className="main-description"
              style={{
                fontSize: "1.2rem",
                maxWidth: "650px",
                margin: "0 auto 3rem",
                color: "#f3e8ff",
                lineHeight: "1.6",
              }}
            >
              Web-Based Implementation of AO
            </p>

            {/* Countdown Display */}
            <div
              className="countdown-grid"
              style={{
                display: "grid",
                gridTemplateColumns: "repeat(4, 1fr)",
                gap: "1.5rem",
                maxWidth: "800px",
                margin: "0 auto 3rem",
              }}
            >
              <div
                className="countdown-box"
                style={{
                  background: "rgba(30, 15, 53, 0.7)",
                  borderRadius: "1rem",
                  padding: "1.5rem",
                  border: "1px solid rgba(194, 82, 255, 0.3)",
                  boxShadow: "0 0 15px rgba(194, 82, 255, 0.3)",
                  backdropFilter: "blur(5px)",
                }}
              >
                <div
                  className="countdown-number"
                  style={{
                    fontFamily: "'Orbitron', sans-serif",
                    fontSize: "3rem",
                    fontWeight: "bold",
                    color: "#d8b4fe",
                    textShadow: "0 0 10px #c252ff",
                  }}
                >
                  {timeLeft.days}
                </div>
                <div
                  className="countdown-label"
                  style={{
                    color: "white",
                    fontWeight: "bold",
                    marginTop: "0.5rem",
                  }}
                >
                  DAYS
                </div>
              </div>
              <div
                className="countdown-box"
                style={{
                  background: "rgba(30, 15, 53, 0.7)",
                  borderRadius: "1rem",
                  padding: "1.5rem",
                  border: "1px solid rgba(194, 82, 255, 0.3)",
                  boxShadow: "0 0 15px rgba(194, 82, 255, 0.3)",
                  backdropFilter: "blur(5px)",
                }}
              >
                <div
                  className="countdown-number"
                  style={{
                    fontFamily: "'Orbitron', sans-serif",
                    fontSize: "3rem",
                    fontWeight: "bold",
                    color: "#d8b4fe",
                    textShadow: "0 0 10px #c252ff",
                  }}
                >
                  {timeLeft.hours}
                </div>
                <div
                  className="countdown-label"
                  style={{
                    color: "white",
                    fontWeight: "bold",
                    marginTop: "0.5rem",
                  }}
                >
                  HOURS
                </div>
              </div>
              <div
                className="countdown-box"
                style={{
                  background: "rgba(30, 15, 53, 0.7)",
                  borderRadius: "1rem",
                  padding: "1.5rem",
                  border: "1px solid rgba(194, 82, 255, 0.3)",
                  boxShadow: "0 0 15px rgba(194, 82, 255, 0.3)",
                  backdropFilter: "blur(5px)",
                }}
              >
                <div
                  className="countdown-number"
                  style={{
                    fontFamily: "'Orbitron', sans-serif",
                    fontSize: "3rem",
                    fontWeight: "bold",
                    color: "#d8b4fe",
                    textShadow: "0 0 10px #c252ff",
                  }}
                >
                  {timeLeft.minutes}
                </div>
                <div
                  className="countdown-label"
                  style={{
                    color: "white",
                    fontWeight: "bold",
                    marginTop: "0.5rem",
                  }}
                >
                  MINUTES
                </div>
              </div>
              <div
                className="countdown-box"
                style={{
                  background: "rgba(30, 15, 53, 0.7)",
                  borderRadius: "1rem",
                  padding: "1.5rem",
                  border: "1px solid rgba(194, 82, 255, 0.3)",
                  boxShadow: "0 0 15px rgba(194, 82, 255, 0.3)",
                  backdropFilter: "blur(5px)",
                }}
              >
                <div
                  className="countdown-number"
                  style={{
                    fontFamily: "'Orbitron', sans-serif",
                    fontSize: "3rem",
                    fontWeight: "bold",
                    color: "#d8b4fe",
                    textShadow: "0 0 10px #c252ff",
                  }}
                >
                  {timeLeft.seconds}
                </div>
                <div
                  className="countdown-label"
                  style={{
                    color: "white",
                    fontWeight: "bold",
                    marginTop: "0.5rem",
                  }}
                >
                  SECONDS
                </div>
              </div>
            </div>

            <a
              href="https://x.com/waoeco"
              target="_blank"
              rel="noopener noreferrer"
            >
              <button
                className="join-button"
                style={{
                  background: "linear-gradient(90deg, #9333EA, #7928CA)",
                  color: "white",
                  border: "none",
                  padding: "0.875rem 1.75rem",
                  fontSize: "1rem",
                  fontWeight: "bold",
                  borderRadius: "9999px",
                  cursor: "pointer",
                  boxShadow: "0 0 15px rgba(194, 82, 255, 0.5)",
                  border: "1px solid rgba(194, 82, 255, 0.5)",
                  transition: "transform 0.3s ease, box-shadow 0.3s ease",
                }}
                onMouseOver={e => {
                  e.currentTarget.style.transform = "translateY(-3px)"
                  e.currentTarget.style.boxShadow =
                    "0 5px 20px rgba(194, 82, 255, 0.7)"
                }}
                onMouseOut={e => {
                  e.currentTarget.style.transform = "translateY(0)"
                  e.currentTarget.style.boxShadow =
                    "0 0 15px rgba(194, 82, 255, 0.5)"
                }}
              >
                Follow The Purple Elephants
              </button>
            </a>
          </div>
        </main>

        {/* Footer */}
        <footer
          style={{
            background:
              "linear-gradient(90deg, rgba(45, 27, 94, 0.9), rgba(61, 29, 111, 0.9))",
            padding: "1.5rem",
            borderTop: "1px solid rgba(194, 82, 255, 0.3)",
            boxShadow: "0 0 15px rgba(194, 82, 255, 0.5)",
            backdropFilter: "blur(5px)",
            position: "relative",
            zIndex: 2,
          }}
        >
          <div
            style={{
              maxWidth: "1200px",
              margin: "0 auto",
              display: "flex",
              justifyContent: "space-between",
              alignItems: "center",
              flexWrap: "wrap",
              gap: "1rem",
              padding: "0 1rem",
            }}
            className="footer-content"
          >
            <div style={{ color: "#d8b4fe" }} className="footer-text">
              &copy; 2025 WAO. All rights reserved.
            </div>
            <div
              style={{
                display: "flex",
                gap: "1.5rem",
                "@media (max-width: 768px)": {
                  justifyContent: "center",
                  width: "100%",
                },
              }}
              className="social-links"
            >
              <a
                href="https://x.com/waoeco"
                target="_blank"
                style={{
                  color: "#d8b4fe",
                  transition:
                    "transform 0.3s ease, color 0.3s ease, filter 0.3s ease",
                  transform: "translateY(0)",
                  display: "block",
                }}
                className="social-icon"
                onMouseOver={e => {
                  e.currentTarget.style.color = "white"
                  e.currentTarget.style.transform = "translateY(-3px)"
                  e.currentTarget.style.filter =
                    "drop-shadow(0 0 5px rgba(194, 82, 255, 0.7))"
                }}
                onMouseOut={e => {
                  e.currentTarget.style.color = "#d8b4fe"
                  e.currentTarget.style.transform = "translateY(0)"
                  e.currentTarget.style.filter = "none"
                }}
              >
                <svg
                  width="20"
                  height="20"
                  viewBox="0 0 24 24"
                  fill="currentColor"
                >
                  <path d="M18.244 2.25h3.308l-7.227 8.26 8.502 11.24H16.17l-5.214-6.817L4.99 21.75H1.68l7.73-8.835L1.254 2.25H8.08l4.713 6.231zm-1.161 17.52h1.833L7.084 4.126H5.117z"></path>
                </svg>
              </a>
              <a
                href="https://github.com/weavedb/wao"
                target="_blank"
                style={{
                  color: "#d8b4fe",
                  transition:
                    "transform 0.3s ease, color 0.3s ease, filter 0.3s ease",
                  transform: "translateY(0)",
                  display: "block",
                }}
                className="social-icon"
                onMouseOver={e => {
                  e.currentTarget.style.color = "white"
                  e.currentTarget.style.transform = "translateY(-3px)"
                  e.currentTarget.style.filter =
                    "drop-shadow(0 0 5px rgba(194, 82, 255, 0.7))"
                }}
                onMouseOut={e => {
                  e.currentTarget.style.color = "#d8b4fe"
                  e.currentTarget.style.transform = "translateY(0)"
                  e.currentTarget.style.filter = "none"
                }}
              >
                <svg
                  width="20"
                  height="20"
                  viewBox="0 0 24 24"
                  fill="currentColor"
                >
                  <path d="M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z"></path>
                </svg>
              </a>
              <a
                href="https://discord.gg/vCkuVhkugY"
                target="_blank"
                style={{
                  color: "#d8b4fe",
                  transition:
                    "transform 0.3s ease, color 0.3s ease, filter 0.3s ease",
                  transform: "translateY(0)",
                  display: "block",
                }}
                className="social-icon"
                onMouseOver={e => {
                  e.currentTarget.style.color = "white"
                  e.currentTarget.style.transform = "translateY(-3px)"
                  e.currentTarget.style.filter =
                    "drop-shadow(0 0 5px rgba(194, 82, 255, 0.7))"
                }}
                onMouseOut={e => {
                  e.currentTarget.style.color = "#d8b4fe"
                  e.currentTarget.style.transform = "translateY(0)"
                  e.currentTarget.style.filter = "none"
                }}
              >
                <svg
                  width="20"
                  height="20"
                  viewBox="0 0 24 24"
                  fill="currentColor"
                >
                  <path d="M20.317 4.3698a19.7913 19.7913 0 00-4.8851-1.5152.0741.0741 0 00-.0785.0371c-.211.3753-.4447.8648-.6083 1.2495-1.8447-.2762-3.68-.2762-5.4868 0-.1636-.3933-.4058-.8742-.6177-1.2495a.077.077 0 00-.0785-.037 19.7363 19.7363 0 00-4.8852 1.515.0699.0699 0 00-.0321.0277C.5334 9.0458-.319 13.5799.0992 18.0578a.0824.0824 0 00.0312.0561c2.0528 1.5076 4.0413 2.4228 5.9929 3.0294a.0777.0777 0 00.0842-.0276c.4616-.6304.8731-1.2952 1.226-1.9942a.076.076 0 00-.0416-.1057c-.6528-.2476-1.2743-.5495-1.8722-.8923a.077.077 0 01-.0076-.1277c.1258-.0943.2517-.1923.3718-.2914a.0743.0743 0 01.0776-.0105c3.9278 1.7933 8.18 1.7933 12.0614 0a.0739.0739 0 01.0785.0095c.1202.099.246.1981.3728.2924a.077.077 0 01-.0066.1276 12.2986 12.2986 0 01-1.873.8914.0766.0766 0 00-.0407.1067c.3604.698.7719 1.3628 1.225 1.9932a.076.076 0 00.0842.0286c1.961-.6067 3.9495-1.5219 6.0023-3.0294a.077.077 0 00.0313-.0552c.5004-5.177-.8382-9.6739-3.5485-13.6604a.061.061 0 00-.0312-.0286zM8.02 15.3312c-1.1825 0-2.1569-1.0857-2.1569-2.419 0-1.3332.9555-2.4189 2.157-2.4189 1.2108 0 2.1757 1.0952 2.1568 2.419 0 1.3332-.9555 2.4189-2.1569 2.4189zm7.9748 0c-1.1825 0-2.1569-1.0857-2.1569-2.419 0-1.3332.9554-2.4189 2.1569-2.4189 1.2108 0 2.1757 1.0952 2.1568 2.419 0 1.3332-.946 2.4189-2.1568 2.4189z"></path>
                </svg>
              </a>
            </div>
          </div>
        </footer>
      </div>
    </>
  )
}
