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
import { getSiteUrlObject, SITE_DESCRIPTION, SITE_TITLE } from "@/lib/site-url";
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

const SITE_URL = getSiteUrlObject();

export const metadata: Metadata = {
  metadataBase: SITE_URL,
  title: {
    default: SITE_TITLE,
    template: "%s · GocciaScript",
  },
  description: SITE_DESCRIPTION,
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
    title: SITE_TITLE,
    description: SITE_DESCRIPTION,
    url: SITE_URL,
    locale: "en_US",
  },
  twitter: {
    card: "summary_large_image",
    title: SITE_TITLE,
    description: SITE_DESCRIPTION,
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
