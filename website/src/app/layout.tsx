import type { Metadata } from "next";
import {
  IBM_Plex_Sans,
  Instrument_Serif,
  JetBrains_Mono,
} from "next/font/google";
import { Suspense } from "react";
import { Analytics } from "@/components/analytics";
import { SiteShell } from "@/components/site-shell";
import { WebMcpTools } from "@/components/webmcp-tools";
import { fetchStars } from "@/lib/github";
import "./globals.css";

const instrumentSerif = Instrument_Serif({
  variable: "--font-instrument-serif",
  subsets: ["latin"],
  weight: "400",
  style: ["normal", "italic"],
  display: "swap",
});

const ibmPlexSans = IBM_Plex_Sans({
  variable: "--font-ibm-plex-sans",
  subsets: ["latin"],
  weight: ["400", "500", "600", "700"],
  display: "swap",
});

const jetbrainsMono = JetBrains_Mono({
  variable: "--font-jetbrains-mono",
  subsets: ["latin"],
  weight: ["400", "500", "700"],
  display: "swap",
});

const DEFAULT_SITE_URL = "https://gocciascript.dev";

// Parse `NEXT_PUBLIC_SITE_URL` defensively — if someone misconfigures it on
// Vercel (typo, missing scheme, leading whitespace), `new URL()` throws at
// module-init time and the entire app fails to boot. Fall back to the
// canonical origin so a bad env var degrades to "metadata points at the
// wrong host" instead of "the site is down".
const SITE_URL = (() => {
  try {
    return new URL(process.env.NEXT_PUBLIC_SITE_URL ?? DEFAULT_SITE_URL);
  } catch {
    return new URL(DEFAULT_SITE_URL);
  }
})();

export const metadata: Metadata = {
  metadataBase: SITE_URL,
  title: {
    default: "GocciaScript — A drop of JavaScript",
    template: "%s · GocciaScript",
  },
  description:
    "A strict subset of ECMAScript 2027+, implemented from scratch — with a sandbox-first runtime designed for tinkerers, embedding and AI agents.",
  applicationName: "GocciaScript",
  keywords: [
    "GocciaScript",
    "JavaScript",
    "ECMAScript",
    "sandbox",
    "AI agents",
    "runtime",
    "interpreter",
    "bytecode",
    "embeddable",
    "FreePascal",
  ],
  authors: [{ name: "Johannes Stein" }],
  creator: "Johannes Stein",
  icons: {
    icon: [{ url: "/logo.png", type: "image/png" }],
    apple: "/logo.png",
  },
  alternates: {
    canonical: "/",
  },
  openGraph: {
    type: "website",
    siteName: "GocciaScript",
    title: "GocciaScript — A drop of JavaScript",
    description:
      "A strict subset of ECMAScript 2027+, implemented from scratch — with a sandbox-first runtime designed for tinkerers, embedding and AI agents.",
    url: SITE_URL,
    locale: "en_US",
  },
  twitter: {
    card: "summary_large_image",
    title: "GocciaScript — A drop of JavaScript",
    description:
      "A strict subset of ECMAScript 2027+, implemented from scratch — with a sandbox-first runtime designed for tinkerers, embedding and AI agents.",
  },
  robots: {
    index: true,
    follow: true,
  },
};

export default async function RootLayout({
  children,
}: Readonly<{ children: React.ReactNode }>) {
  const stars = await fetchStars();
  return (
    <html
      lang="en"
      data-accent="amber"
      className={`${instrumentSerif.variable} ${ibmPlexSans.variable} ${jetbrainsMono.variable}`}
      suppressHydrationWarning
    >
      <head>
        <script
          // biome-ignore lint/security/noDangerouslySetInnerHtml: pre-paint theme bootstrap, no user data
          dangerouslySetInnerHTML={{
            __html: `(function(){try{var m=window.matchMedia('(prefers-color-scheme: dark)');document.documentElement.dataset.theme=m.matches?'espresso':'cream';}catch(e){}})();`,
          }}
        />
      </head>
      <body data-grain="true">
        <SiteShell stars={stars}>{children}</SiteShell>
        {/* `<Analytics>` reads `useSearchParams()` for page-view URLs,
            which Next requires inside `<Suspense>` for a stable SSR
            boundary. Covers page views, web vitals, and uncaught
            exceptions — see `components/analytics.tsx`. */}
        <Suspense fallback={null}>
          <Analytics />
        </Suspense>
        <WebMcpTools />
      </body>
    </html>
  );
}
