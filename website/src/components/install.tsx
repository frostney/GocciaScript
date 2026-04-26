"use client";

import Link from "next/link";
import { type ComponentType, useEffect, useState } from "react";
import { AnchorH2 } from "@/components/anchor-heading";
import { HighlightedGeneric } from "@/components/highlighted-code";
import {
  BookIcon,
  BunIcon,
  CopyIcon,
  GithubIcon,
  NpmIcon,
  PnpmIcon,
} from "@/components/icons";
import {
  GITHUB_RELEASES_URL,
  GITHUB_REPO_URL,
  isPreStable,
  type ReleaseInfo,
} from "@/lib/github";

type Method = {
  id: string;
  label: string;
  description: string;
  command: string;
  language?: "shell" | "ts";
};

type TabSpec = {
  key: string;
  label: string;
  Icon?: ComponentType<{ size?: number }>;
};

/** Operating-system tabs for the Quick install card. macOS and Linux
 *  both run the same `curl … | sh` one-liner but get separate tabs so
 *  users see their platform reflected explicitly. */
const OS_TABS: readonly TabSpec[] = [
  { key: "macos", label: "macOS" },
  { key: "linux", label: "Linux" },
  { key: "windows", label: "Windows" },
];

/** Package-manager tabs for the npm / Bun / pnpm sections — brand
 *  icons in their official colors so the active selection is
 *  recognizable at a glance. */
const PM_TABS: readonly TabSpec[] = [
  { key: "npm", label: "npm", Icon: NpmIcon },
  { key: "bun", label: "Bun", Icon: BunIcon },
  { key: "pnpm", label: "pnpm", Icon: PnpmIcon },
];

const METHODS: Method[] = [
  {
    id: "homebrew",
    label: "Homebrew · macOS",
    description:
      "Tap the formula and brew install — keeps you on the latest stable.",
    command: "brew install frostney/tap/gocciascript",
    language: "shell",
  },
  {
    id: "apt",
    label: "Linux · Debian/Ubuntu",
    description: "Add the apt repo and install the .deb.",
    command:
      "curl -fsSL https://gocciascript.dev/repo/key.gpg | sudo gpg --dearmor -o /usr/share/keyrings/gocciascript.gpg\necho 'deb [signed-by=/usr/share/keyrings/gocciascript.gpg] https://gocciascript.dev/repo stable main' | sudo tee /etc/apt/sources.list.d/gocciascript.list\nsudo apt update && sudo apt install gocciascript",
    language: "shell",
  },
  {
    id: "scoop",
    label: "Windows · Scoop",
    description:
      "If you prefer a package manager: add the bucket, then scoop install. Stays on the latest stable.",
    command:
      "scoop bucket add frostney https://github.com/frostney/scoop-bucket\nscoop install gocciascript",
    language: "shell",
  },
  {
    id: "binaries",
    label: "Pre-built binaries",
    description:
      "Download the binary for your OS/arch from the GitHub release and drop it anywhere on your $PATH.",
    command:
      "# darwin-arm64 / darwin-x86_64 / linux-x86_64 / linux-aarch64 / windows-x86_64\ncurl -fsSL -o GocciaScriptLoader \\\n  https://github.com/frostney/GocciaScript/releases/latest/download/GocciaScriptLoader-darwin-arm64\nchmod +x GocciaScriptLoader\nmv GocciaScriptLoader /usr/local/bin/",
    language: "shell",
  },
  {
    id: "from-source",
    label: "Build from source",
    description:
      "FreePascal compiler required. Clone, run the build script, point your $PATH at ./build.",
    command:
      "git clone https://github.com/frostney/GocciaScript\ncd GocciaScript\n./build.pas loader\n./build/GocciaScriptLoader --help",
    language: "shell",
  },
];

/** Tabbed command picker — mirrors the docs-site pattern used by
 *  sli.dev, vite.dev, etc. Used for both the OS picker (Quick install
 *  card) and the package-manager picker (npm / Bun / pnpm sections).
 *  Active selection persists in `localStorage` per `storageKey` and
 *  synchronizes across sibling groups via a custom `cmd-tabs-sync`
 *  event — picking "Bun" on the install section flips the run-without-
 *  install section to Bun too. */
function CommandTabs({
  tabs,
  commands,
  storageKey,
}: {
  tabs: readonly TabSpec[];
  commands: Record<string, string>;
  /** When set, the active tab is persisted across visits and shared
   *  with other groups using the same key on this page. */
  storageKey?: string;
}) {
  const initialKey = tabs[0]?.key ?? "";
  const [active, setActive] = useState<string>(initialKey);

  useEffect(() => {
    if (!storageKey || typeof window === "undefined") return;
    const isKnown = (v: unknown): v is string =>
      typeof v === "string" && tabs.some((t) => t.key === v);
    try {
      const saved = window.localStorage.getItem(storageKey);
      if (isKnown(saved)) setActive(saved);
    } catch {}
    const onSync = (e: Event) => {
      const detail = (e as CustomEvent<{ key: string; value: string }>).detail;
      if (detail?.key !== storageKey) return;
      if (isKnown(detail.value)) setActive(detail.value);
    };
    window.addEventListener("cmd-tabs-sync", onSync as EventListener);
    return () =>
      window.removeEventListener("cmd-tabs-sync", onSync as EventListener);
  }, [storageKey, tabs]);

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

function CopyableCommand({
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
          <HighlightedGeneric code={command} language={language} />
        </code>
      </pre>
    </div>
  );
}

export function Install({ release }: { release: ReleaseInfo | null }) {
  return (
    <div className="pt-16 pb-24">
      <div className="container">
        <div className="install-hero">
          <div className="install-hero-text">
            <div className="section-head">
              <div className="section-kicker">Install</div>
              <h1>
                Get Goccia<em>Script</em> on your machine.
              </h1>
              <p>
                Pick the install method that fits your platform. The runtime is
                a single self-contained binary — no Node.js, no toolchain, no
                global state.
              </p>
            </div>

            {isPreStable(release) && (
              <div className="prestable-banner" role="note">
                <strong>Pre-1.0 release.</strong> The public API is still being
                shaped — anything documented can change between any two releases
                until <code>1.0.0</code> ships. Pin a specific version in CI and
                check the{" "}
                <a
                  href={GITHUB_RELEASES_URL}
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  release notes
                </a>{" "}
                before upgrading.
              </div>
            )}

            <div className="install-meta">
              {release ? (
                <p className="install-latest">
                  Latest version{" "}
                  <a
                    href={release.htmlUrl}
                    target="_blank"
                    rel="noopener noreferrer"
                    title={release.name ?? release.tagName}
                  >
                    <strong>{release.tagName}</strong>
                  </a>
                  {release.publishedAt && (
                    <>
                      {" "}
                      released{" "}
                      {new Date(release.publishedAt).toLocaleDateString(
                        undefined,
                        {
                          year: "numeric",
                          month: "long",
                          day: "numeric",
                        },
                      )}
                    </>
                  )}
                </p>
              ) : (
                <p className="install-latest">Latest version —</p>
              )}
              <div className="install-meta-links">
                <a
                  href={GITHUB_RELEASES_URL}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="install-link"
                >
                  <BookIcon size={14} /> Release notes
                </a>
                <a
                  href={GITHUB_REPO_URL}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="install-link"
                >
                  <GithubIcon size={14} /> Source on GitHub
                </a>
              </div>
            </div>
          </div>

          <aside className="install-quick" aria-labelledby="install-quick-h">
            <h3 id="install-quick-h">Quick install</h3>
            <p>
              One-liner installer — fetches the right binary for your platform
              and drops it on your <code>$PATH</code>.
            </p>
            <CommandTabs
              tabs={OS_TABS}
              storageKey="goccia.install.os"
              commands={{
                macos: "curl -fsSL https://gocciascript.dev/install.sh | sh",
                linux: "curl -fsSL https://gocciascript.dev/install.sh | sh",
                windows: "irm https://gocciascript.dev/install.ps1 | iex",
              }}
            />
          </aside>
        </div>

        <div className="install-methods">
          {METHODS.flatMap((m) => {
            const sections = [
              <section key={m.id} id={m.id} className="install-method">
                <AnchorH2 id={m.id}>{m.label}</AnchorH2>
                <p>{m.description}</p>
                <CopyableCommand command={m.command} language={m.language} />
              </section>,
            ];
            // Inject the JS-package-manager tabbed sections after the
            // OS package managers and before the pre-built-binaries
            // method, so all "managed install" paths sit together.
            // Order: zero-cost run-without-install → global install.
            if (m.id === "scoop") {
              sections.push(
                <section key="npm-run" id="npx" className="install-method">
                  <AnchorH2 id="npx">Run without installing</AnchorH2>
                  <p>
                    For one-off scripts and CI tasks where you don&apos;t want a
                    global install. The package is fetched into the
                    manager&apos;s cache on first run; subsequent invocations
                    are essentially free.
                  </p>
                  <CommandTabs
                    tabs={PM_TABS}
                    storageKey="goccia.install.pm"
                    commands={{
                      npm: "npx gocciascript script.js",
                      bun: "bunx gocciascript script.js",
                      pnpm: "pnpm dlx gocciascript script.js",
                    }}
                  />
                </section>,
                <section key="npm-install" id="npm" className="install-method">
                  <AnchorH2 id="npm">JavaScript package managers</AnchorH2>
                  <p>
                    Cross-platform global install via npm, Bun, or pnpm. The npm
                    package downloads the right native binary for your OS / arch
                    on install — same pattern as esbuild and biome.
                  </p>
                  <CommandTabs
                    tabs={PM_TABS}
                    storageKey="goccia.install.pm"
                    commands={{
                      npm: "npm install -g gocciascript",
                      bun: "bun install -g gocciascript",
                      pnpm: "pnpm add -g gocciascript",
                    }}
                  />
                </section>,
              );
            }
            return sections;
          })}
        </div>

        <div className="install-next">
          <h2>Next</h2>
          <p>
            <Link href="/docs">Read the docs</Link> for a full language tour, or
            jump straight into the{" "}
            <Link href="/playground?example=coffee-typed">Playground</Link> to
            try the runtime in your browser.
          </p>
        </div>
      </div>
    </div>
  );
}
