"use client";

import { usePathname, useSearchParams } from "next/navigation";
import posthog from "posthog-js";
import { useEffect } from "react";

const POSTHOG_KEY = process.env.NEXT_PUBLIC_POSTHOG_KEY;
const POSTHOG_HOST =
  process.env.NEXT_PUBLIC_POSTHOG_HOST ?? "https://eu.i.posthog.com";

/** Single PostHog client that covers everything we previously had
 *  three packages for:
 *
 *   - **Page-view analytics** — manual `$pageview` on each
 *     `usePathname()` / `useSearchParams()` change because Next.js
 *     App Router client transitions don't trigger a full page load
 *     and PostHog's autocapture doesn't pick them up reliably.
 *   - **Web Vitals** (replaces `@vercel/speed-insights`) — captured
 *     by PostHog's performance hook when enabled via
 *     `capture_performance.web_vitals`. Same LCP / FCP / CLS / INP /
 *     TTFB measurements, surfaced in PostHog's Web Analytics dashboard.
 *   - **Exception tracking** (replaces an external Sentry / Vercel
 *     error monitor) — `capture_exceptions: true` hooks
 *     `window.onerror` and `unhandledrejection` so uncaught JS errors
 *     show up in PostHog's Error Tracking view automatically.
 *
 *  No-ops cleanly when `NEXT_PUBLIC_POSTHOG_KEY` is unset, so local
 *  dev and forks without analytics keys keep working. */
export function Analytics() {
  const pathname = usePathname();
  const searchParams = useSearchParams();

  useEffect(() => {
    if (!POSTHOG_KEY || typeof window === "undefined") return;
    if (posthog.__loaded) return;
    posthog.init(POSTHOG_KEY, {
      api_host: POSTHOG_HOST,
      // We dispatch `$pageview` manually below.
      capture_pageview: false,
      // Privacy posture: no form / click autocapture by default; we
      // identify users only when explicitly opted in via product
      // surfaces (we don't have any yet).
      autocapture: false,
      person_profiles: "identified_only",
      // Web Vitals replaces Vercel Speed Insights — PostHog's
      // `web-vitals` integration measures LCP / FCP / CLS / INP / TTFB
      // and reports them as `$web_vitals` events.
      capture_performance: { web_vitals: true },
      // Error tracking replaces Sentry. PostHog hooks
      // `window.onerror` and `unhandledrejection` and reports them as
      // `$exception` events visible in the Error Tracking dashboard.
      capture_exceptions: true,
    });
  }, []);

  useEffect(() => {
    if (!POSTHOG_KEY || typeof window === "undefined") return;
    if (!posthog.__loaded) return;
    let url = window.origin + pathname;
    const qs = searchParams?.toString();
    if (qs) url += `?${qs}`;
    posthog.capture("$pageview", { $current_url: url });
  }, [pathname, searchParams]);

  return null;
}
