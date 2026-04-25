"use client";

import Link from "next/link";
import { useEffect, useState } from "react";
import { AnchorH2 } from "@/components/anchor-heading";
import { HighlightedGeneric } from "@/components/highlighted-code";
import { BookIcon, CopyIcon, GithubIcon } from "@/components/icons";
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

/** The "Quick install" one-liners shown in the right-side hero card —
 *  not in the methods list, since they're the recommended path and
 *  belong above the fold. */
const QUICK_INSTALLS: {
  id: string;
  label: string;
  command: string;
  hint?: string;
}[] = [
  {
    id: "quick-unix",
    label: "macOS / Linux",
    command: "curl -fsSL https://gocciascript.dev/install.sh | sh",
  },
  {
    id: "quick-windows",
    label: "Windows · PowerShell",
    command: "irm https://gocciascript.dev/install.ps1 | iex",
  },
  {
    id: "quick-npx",
    label: "Run without installing",
    command: "npx gocciascript script.js",
    hint: "Bun: `bunx gocciascript` · pnpm: `pnpm dlx gocciascript`.",
  },
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
    id: "npm",
    label: "npm · Bun · pnpm",
    description:
      "Cross-platform install through the JavaScript package managers. The npm package downloads the right native binary for your OS/arch on install.",
    command:
      "# npm\nnpm install -g gocciascript\n\n# Bun\nbun install -g gocciascript\n\n# pnpm\npnpm add -g gocciascript",
    language: "shell",
  },
  {
    id: "npx",
    label: "Run without installing — npx · bunx · pnpm dlx",
    description:
      "Use this for one-off scripts and CI tasks where you don't want a global install. The package is fetched into the manager's cache on first run; subsequent invocations are essentially free.",
    command:
      "# npx — included with Node.js\nnpx gocciascript script.js\n\n# bunx — bundled with Bun\nbunx gocciascript script.js\n\n# pnpm dlx\npnpm dlx gocciascript script.js",
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
              <h1>Get GocciaScript on your machine.</h1>
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
              One-liner installers — they detect your platform, fetch the right
              binary, and drop it on your <code>$PATH</code>.
            </p>
            {QUICK_INSTALLS.map((q) => (
              <div key={q.id} className="install-quick-row">
                <div className="install-quick-label">{q.label}</div>
                <CopyableCommand command={q.command} language="shell" />
                {q.hint && (
                  <p className="install-quick-hint">
                    {q.hint
                      .split(/(`[^`]+`)/)
                      .map((part, i) =>
                        part.startsWith("`") && part.endsWith("`") ? (
                          <code key={i}>{part.slice(1, -1)}</code>
                        ) : (
                          <span key={i}>{part}</span>
                        ),
                      )}
                  </p>
                )}
              </div>
            ))}
          </aside>
        </div>

        <div className="install-methods">
          {METHODS.map((m) => (
            <section key={m.id} id={m.id} className="install-method">
              <AnchorH2 id={m.id}>{m.label}</AnchorH2>
              <p>{m.description}</p>
              <CopyableCommand command={m.command} language={m.language} />
            </section>
          ))}
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
