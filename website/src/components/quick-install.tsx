"use client";

import { useEffect, useState } from "react";
import {
  CommandTabs,
  detectOs,
  OS_TABS,
  type OsKey,
} from "@/components/command-tabs";
import { CANONICAL_SITE_URL } from "@/lib/site-url";

/** OS-tabbed Quick install — the canonical "fastest path to running"
 *  surface, designed to be embedded in both the dedicated `/install`
 *  page and the homepage. Auto-detects the user's platform from
 *  `navigator.userAgent` after hydration and pre-selects that tab.
 *  Persisted user choice in `localStorage` (under `goccia.install.os`)
 *  always wins over the auto-detect. */
export function QuickInstall() {
  // Static fallback for SSR — the client useEffect overrides on first
  // render so the tab matches the user's actual OS. We keep the static
  // key stable so the SSR markup is deterministic.
  const [detectedOs, setDetectedOs] = useState<OsKey>("macos");
  useEffect(() => {
    if (typeof navigator === "undefined") return;
    setDetectedOs(detectOs(navigator.userAgent));
  }, []);

  return (
    <CommandTabs
      tabs={OS_TABS}
      storageKey="goccia.install.os"
      initialKey={detectedOs}
      commands={{
        macos: `curl -fsSL ${CANONICAL_SITE_URL}/install | sh`,
        linux: `curl -fsSL ${CANONICAL_SITE_URL}/install | sh`,
        windows: `irm ${CANONICAL_SITE_URL}/install.ps1 | iex`,
      }}
    />
  );
}
