"use client";

import { usePathname, useSearchParams } from "next/navigation";
import posthog from "posthog-js";
import { useEffect } from "react";

const POSTHOG_KEY = process.env.NEXT_PUBLIC_POSTHOG_KEY;
const POSTHOG_HOST =
  process.env.NEXT_PUBLIC_POSTHOG_HOST ?? "https://eu.i.posthog.com";

/** PostHog page-view tracker. Initialises the SDK once on mount when
 *  `NEXT_PUBLIC_POSTHOG_KEY` is set; no-ops otherwise so local
 *  development and forks without analytics keys keep working.
 *
 *  Manual page-view capture (instead of the SDK's default
 *  pageview-on-load behaviour) is needed because Next.js client
 *  navigation doesn't trigger a full page load — the App Router
 *  route changes update `usePathname()` but PostHog's autocapture
 *  doesn't pick that up reliably. We capture explicitly on each
 *  pathname / search-params change. */
export function Analytics() {
  const pathname = usePathname();
  const searchParams = useSearchParams();

  useEffect(() => {
    if (!POSTHOG_KEY || typeof window === "undefined") return;
    if (posthog.__loaded) return;
    posthog.init(POSTHOG_KEY, {
      api_host: POSTHOG_HOST,
      // We capture pageviews ourselves below.
      capture_pageview: false,
      // Privacy: don't auto-capture form values etc. by default.
      autocapture: false,
      person_profiles: "identified_only",
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
