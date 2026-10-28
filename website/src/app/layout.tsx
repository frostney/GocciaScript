import type { Metadata } from "next";
import { Suspense } from "react";
import { Analytics } from "@/components/analytics";
import { SiteShell } from "@/components/site-shell";
import { WebMcpTools } from "@/components/webmcp-tools";
import "@/lib/fonts";
import { getSiteUrlObject, SITE_DESCRIPTION, SITE_TITLE } from "@/lib/site-url";
import "./globals.css";

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
    "bytecode",
    "virtual machine",
    "embeddable",
    "FreePascal",
    "Delphi",
    "JavaScript engine",
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

export default function RootLayout({
  children,
}: Readonly<{ children: React.ReactNode }>) {
  return (
    <html lang="en" data-accent="amber" suppressHydrationWarning>
      <body data-grain="true">
        <SiteShell>{children}</SiteShell>
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
