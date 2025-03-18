import { Html, Main, NextScript } from "next/document"
import { Head } from "arnext"

export default function Document() {
  const title = "WAO | DevNet Launch"
  const desc = "Web-Based Implementation of AO"
  const image = "https://wao.eco/cover.png"
  return (
    <Html lang="en" suppressHydrationWarning>
      <Head>
        <meta name="twitter:card" content="summary_large_image" />
        <meta name="twitter:title" content={title} />
        <meta name="twitter:description" content={desc} />
        <meta name="twitter:image" content={image} />
        <meta property="og:title" content={title} />
        <meta name="og:description" content={desc} />
        <meta name="og:image" content={image} />
      </Head>
      <body>
        <Main />
        <NextScript />
      </body>
    </Html>
  )
}
