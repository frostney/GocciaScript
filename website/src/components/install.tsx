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

const METHODS: Method[] = [
  {
    id: "curl",
    label: "Quick install",
    description:
      "One-liner installer — downloads the right binary for your platform and drops it on your $PATH.",
    command: "curl -fsSL https://gocciascript.dev/install.sh | sh",
    language: "shell",
  },
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

function CopyableCommand({ command }: { command: string }) {
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
          <HighlightedGeneric code={command} language="ts" />
        </code>
      </pre>
    </div>
  );
}

export function Install({ release }: { release: ReleaseInfo | null }) {
  return (
    <div className="pt-16 pb-24">
      <div className="container">
        <div className="section-head">
          <div className="section-kicker">Install</div>
          <h1>Get GocciaScript on your machine.</h1>
          <p>
            Pick the install method that fits your platform. The runtime is a
            single self-contained binary — no Node.js, no toolchain, no global
            state.
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
            <a
              href={release.htmlUrl}
              target="_blank"
              rel="noopener noreferrer"
              className="install-version install-version-large"
              title={release.name ?? release.tagName}
            >
              <span>Latest release</span>
              <strong>{release.tagName}</strong>
              {release.publishedAt && (
                <span className="install-version-date">
                  {new Date(release.publishedAt).toLocaleDateString(undefined, {
                    year: "numeric",
                    month: "short",
                    day: "numeric",
                  })}
                </span>
              )}
            </a>
          ) : (
            <span className="install-version install-version-large">
              <span>Latest release</span>
              <strong>—</strong>
            </span>
          )}
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

        <div className="install-methods">
          {METHODS.map((m) => (
            <section key={m.id} id={m.id} className="install-method">
              <AnchorH2 id={m.id}>{m.label}</AnchorH2>
              <p>{m.description}</p>
              <CopyableCommand command={m.command} />
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
