"use client";

import { type ComponentType, useEffect, useState } from "react";
import {
  HighlightedGeneric,
  HighlightedShell,
} from "@/components/highlighted-code";
import {
  AppleIcon,
  BunIcon,
  CopyIcon,
  DenoIcon,
  LinuxIcon,
  NpmIcon,
  PnpmIcon,
  WindowsIcon,
  YarnIcon,
} from "@/components/icons";

export type TabSpec = {
  key: string;
  label: string;
  Icon?: ComponentType<{ size?: number }>;
};

export type OsKey = "macos" | "linux" | "windows";

/** Operating-system tabs — used by Quick install, native package
 *  manager, pre-built binaries, and build-from-source sections. */
export const OS_TABS: readonly TabSpec[] = [
  { key: "macos", label: "macOS", Icon: AppleIcon },
  { key: "linux", label: "Linux", Icon: LinuxIcon },
  { key: "windows", label: "Windows", Icon: WindowsIcon },
];

/** JS-package-manager tabs — npm / Bun / pnpm / Yarn / Deno. The
 *  active selection is shared (via the same `storageKey`) across both
 *  the install and run-without-install sections so a user picks
 *  their PM once and it sticks. */
export const PM_TABS: readonly TabSpec[] = [
  { key: "npm", label: "npm", Icon: NpmIcon },
  { key: "bun", label: "Bun", Icon: BunIcon },
  { key: "pnpm", label: "pnpm", Icon: PnpmIcon },
  { key: "yarn", label: "Yarn", Icon: YarnIcon },
  { key: "deno", label: "Deno", Icon: DenoIcon },
];

/** Best-guess `OsKey` from a navigator user-agent string. Falls back
 *  to `"macos"` when the UA can't be classified — most likely on
 *  embedded WebViews where the user is heading to a desktop machine
 *  next anyway. */
export function detectOs(ua: string): OsKey {
  const s = ua.toLowerCase();
  if (s.includes("windows")) return "windows";
  if (s.includes("mac") || s.includes("iphone") || s.includes("ipad")) {
    return "macos";
  }
  if (
    s.includes("linux") ||
    s.includes("android") ||
    s.includes("x11") ||
    s.includes("freebsd") ||
    s.includes("openbsd")
  ) {
    return "linux";
  }
  return "macos";
}

/** Architecture identifiers — match the suffixes used in the GitHub
 *  release archive names (`gocciascript-{version}-{os}-{arch}.{ext}`):
 *    macos:   arm64 | x64
 *    linux:   arm64 | x64
 *    windows: x64   | x86
 *  We use one union for simplicity; per-OS code clamps to the legal set. */
export type ArchKey = "arm64" | "x64" | "x86";

/** Platform-aware label set for the ⌘/Ctrl+Enter Run shortcut. The
 *  same physical key combo is bound on every platform — the keydown
 *  handlers all check `e.metaKey || e.ctrlKey` — but the symbol the
 *  UI displays for it differs: macOS users expect `⌘`, everyone else
 *  expects `Ctrl`. `short` is the compact form for inline use inside
 *  buttons (matches the visual weight of `⌘↵`); `long` is the expanded
 *  form for tooltips and prose. */
export type RunShortcut = {
  short: string;
  long: string;
};

const RUN_SHORTCUT_MAC: RunShortcut = { short: "⌘↵", long: "⌘+Enter" };
const RUN_SHORTCUT_NON_MAC: RunShortcut = {
  short: "Ctrl+↵",
  long: "Ctrl+Enter",
};

/** Returns the platform-appropriate Run-shortcut label.
 *
 *  Hydration-safe: SSR (and the first client paint) renders the
 *  macOS form so the markup is deterministic, then a `useEffect`
 *  re-detects on the client via `navigator.userAgent` — same shape
 *  as `<QuickInstall>`'s OS auto-pick. Users on Linux/Windows see a
 *  brief flash of `⌘` before it swaps to `Ctrl` on first paint;
 *  acceptable trade-off for not blocking SSR on user-agent inspection. */
export function useRunShortcut(): RunShortcut {
  const [shortcut, setShortcut] = useState<RunShortcut>(RUN_SHORTCUT_MAC);
  useEffect(() => {
    if (typeof navigator === "undefined") return;
    setShortcut(
      detectOs(navigator.userAgent) === "macos"
        ? RUN_SHORTCUT_MAC
        : RUN_SHORTCUT_NON_MAC,
    );
  }, []);
  return shortcut;
}

/** Best-guess CPU architecture from a navigator user-agent string and
 *  an optional `os` hint. Default-pick reflects current desktop sales:
 *  arm64 for Apple Silicon Macs (the default for new hardware since
 *  late 2020), x64 for everything else.
 *
 *  Note: Apple Silicon Safari/WebKit hides the architecture in the UA
 *  string (always reports "Intel Mac OS X" for fingerprinting reasons),
 *  so without `navigator.userAgentData` we can only pick the most
 *  likely default. The user can switch via the commented-out alternate
 *  arch line in the rendered command. */
export function detectArch(ua: string, os: OsKey = detectOs(ua)): ArchKey {
  const s = ua.toLowerCase();
  if (
    s.includes("aarch64") ||
    s.includes("arm64") ||
    s.includes(" arm;") ||
    s.includes(" arm)")
  ) {
    return "arm64";
  }
  if (s.includes("x86_64") || s.includes("x64") || s.includes("wow64")) {
    return "x64";
  }
  // 32-bit Windows is rare today but real on long-lived machines.
  if (os === "windows" && (s.includes("win32") || s.includes(";x86"))) {
    return "x86";
  }
  // macOS: arm64 is the safer default for new Macs (Apple Silicon).
  if (os === "macos") return "arm64";
  return "x64";
}

/** A copy-to-clipboard button + syntax-highlighted command pre. Routes
 *  shell commands through the dedicated shell tokenizer so URLs aren't
 *  mistaken for `//`-style comments by the C-family generic highlighter. */
export function CopyableCommand({
  command,
  language = "shell",
}: {
  command: string;
  language?: "shell" | "ts";
}) {
  const [copyTick, setCopyTick] = useState(0);
  const copy = async () => {
    let ok = false;
    try {
      if (navigator.clipboard?.writeText) {
        await navigator.clipboard.writeText(command);
        ok = true;
      }
    } catch {}
    if (ok) setCopyTick((t) => t + 1);
  };
  useEffect(() => {
    if (copyTick === 0) return;
    const id = setTimeout(() => setCopyTick(0), 1500);
    return () => clearTimeout(id);
  }, [copyTick]);
  const copied = copyTick > 0;
  return (
    <div className="install-block">
      <button
        type="button"
        className="install-block-copy"
        onClick={copy}
        title="Copy"
        aria-label="Copy command"
      >
        {copied ? (
          <span key={copyTick} className="copied-flash">
            copied
          </span>
        ) : (
          <CopyIcon size={14} />
        )}
      </button>
      <pre className="install-block-pre">
        <code>
          {language === "shell" ? (
            <HighlightedShell code={command} />
          ) : (
            <HighlightedGeneric code={command} language={language} />
          )}
        </code>
      </pre>
    </div>
  );
}

/** Tabbed command picker — sli.dev / vite.dev style. Used for both OS
 *  and PM pickers across the site. Active selection persists in
 *  `localStorage` per `storageKey` and synchronizes across sibling
 *  groups via a custom `cmd-tabs-sync` event — picking "Bun" on the
 *  install section flips the run-without-install section to Bun too. */
export function CommandTabs({
  tabs,
  commands,
  storageKey,
  initialKey,
}: {
  tabs: readonly TabSpec[];
  commands: Record<string, string>;
  /** When set, the active tab is persisted across visits and shared
   *  with other groups using the same key on this page. */
  storageKey?: string;
  /** Optional client-side default applied after hydration when no
   *  persisted selection exists. Used by the OS picker to honor the
   *  user's actual platform on first visit. */
  initialKey?: string;
}) {
  const [active, setActive] = useState<string>(tabs[0]?.key ?? "");

  useEffect(() => {
    if (typeof window === "undefined") return;
    const isKnown = (v: unknown): v is string =>
      typeof v === "string" && tabs.some((t) => t.key === v);
    let resolved = false;
    if (storageKey) {
      try {
        const saved = window.localStorage.getItem(storageKey);
        if (isKnown(saved)) {
          setActive(saved);
          resolved = true;
        }
      } catch {}
    }
    if (!resolved && isKnown(initialKey)) setActive(initialKey);
    if (!storageKey) return;
    const onSync = (e: Event) => {
      const detail = (e as CustomEvent<{ key: string; value: string }>).detail;
      if (detail?.key !== storageKey) return;
      if (isKnown(detail.value)) setActive(detail.value);
    };
    window.addEventListener("cmd-tabs-sync", onSync as EventListener);
    return () =>
      window.removeEventListener("cmd-tabs-sync", onSync as EventListener);
  }, [storageKey, tabs, initialKey]);

  const select = (key: string) => {
    setActive(key);
    if (storageKey && typeof window !== "undefined") {
      try {
        window.localStorage.setItem(storageKey, key);
      } catch {}
      window.dispatchEvent(
        new CustomEvent("cmd-tabs-sync", {
          detail: { key: storageKey, value: key },
        }),
      );
    }
  };

  return (
    <div className="cmd-tabs-wrap">
      <div className="cmd-tabs" role="tablist">
        {tabs.map((t) => {
          const selected = active === t.key;
          return (
            <button
              key={t.key}
              type="button"
              role="tab"
              aria-selected={selected}
              tabIndex={selected ? 0 : -1}
              className="cmd-tab"
              data-key={t.key}
              data-active={selected}
              onClick={() => select(t.key)}
            >
              {t.Icon && <t.Icon size={16} />}
              <span>{t.label}</span>
            </button>
          );
        })}
      </div>
      <div role="tabpanel">
        <CopyableCommand command={commands[active] ?? ""} language="shell" />
      </div>
    </div>
  );
}
