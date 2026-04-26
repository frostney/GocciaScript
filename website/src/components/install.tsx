"use client";

import Link from "next/link";
import { useEffect, useState } from "react";
import { AnchorH2 } from "@/components/anchor-heading";
import {
  type ArchKey,
  CommandTabs,
  detectArch,
  OS_TABS,
  PM_TABS,
} from "@/components/command-tabs";
import { BookIcon, GithubIcon } from "@/components/icons";
import { LatestVersion } from "@/components/latest-version";
import { QuickInstall } from "@/components/quick-install";
import {
  GITHUB_RELEASES_URL,
  GITHUB_REPO_URL,
  isPreStable,
  type ReleaseInfo,
} from "@/lib/github";

/** Native package-manager commands per OS. The single "system PM"
 *  method below uses these as its OS-tabbed payload — saves the user
 *  scrolling past three OS-specific entries to find theirs. */
const SYSTEM_PM_COMMANDS = {
  macos: "brew install frostney/tap/gocciascript",
  linux:
    "curl -fsSL https://gocciascript.dev/repo/key.gpg | sudo gpg --dearmor -o /usr/share/keyrings/gocciascript.gpg\necho 'deb [signed-by=/usr/share/keyrings/gocciascript.gpg] https://gocciascript.dev/repo stable main' | sudo tee /etc/apt/sources.list.d/gocciascript.list\nsudo apt update && sudo apt install gocciascript",
  windows:
    "scoop bucket add frostney https://github.com/frostney/scoop-bucket\nscoop install gocciascript",
} as const;

/** Pre-built binary download instructions per OS. Includes all three
 *  shipped executables (loader / test runner / REPL) so the user
 *  ends up with the full toolchain on $PATH, not just the script
 *  loader. The macOS / Linux variants render BOTH arm64 and x86_64
 *  blocks, with the detected arch active and the other commented out
 *  — flip by uncommenting the lines you want. */
const RELEASES_BASE =
  "https://github.com/frostney/GocciaScript/releases/latest/download";

const ARCH_LABELS = {
  macos: { arm64: "Apple Silicon (arm64)", x86_64: "Intel (x86_64)" },
  linux: { arm64: "ARM (aarch64)", x86_64: "x86_64" },
} as const;
const ARCH_SUFFIX = {
  macos: { arm64: "darwin-arm64", x86_64: "darwin-x86_64" },
  linux: { arm64: "linux-aarch64", x86_64: "linux-x86_64" },
} as const;

/** Build the Unix prebuilt block for a given OS at a given arch. The
 *  active arch lines are uncommented; the alternate arch is shown
 *  fully commented underneath so the user can swap by toggling
 *  comments. */
function unixPrebuiltBlock(
  os: "macos" | "linux",
  active: ArchKey,
  commented: boolean,
): string {
  const suffix = ARCH_SUFFIX[os][active];
  const label = ARCH_LABELS[os][active];
  const c = commented ? "# " : "";
  const tag = commented ? "uncomment to use instead" : "auto-detected";
  return [
    `# ${label} — ${tag}`,
    `${c}for bin in GocciaScriptLoader GocciaTestRunner GocciaREPL; do`,
    `${c}  curl -fsSL -o "$bin" "${RELEASES_BASE}/$bin-${suffix}"`,
    `${c}  chmod +x "$bin"`,
    `${c}done`,
    `${c}sudo mv GocciaScriptLoader GocciaTestRunner GocciaREPL /usr/local/bin/`,
  ].join("\n");
}

function buildPrebuiltCommands(arch: ArchKey) {
  const otherArch: ArchKey = arch === "arm64" ? "x86_64" : "arm64";
  return {
    macos: [
      unixPrebuiltBlock("macos", arch, false),
      "",
      unixPrebuiltBlock("macos", otherArch, true),
    ].join("\n"),
    linux: [
      unixPrebuiltBlock("linux", arch, false),
      "",
      unixPrebuiltBlock("linux", otherArch, true),
    ].join("\n"),
    windows: [
      "# Save under a folder on your PATH (creates one if needed)",
      '$dest = "$env:USERPROFILE\\bin"',
      "New-Item -ItemType Directory -Force -Path $dest | Out-Null",
      "",
      'foreach ($exe in @("GocciaScriptLoader", "GocciaTestRunner", "GocciaREPL")) {',
      `  Invoke-WebRequest -Uri "${RELEASES_BASE}/$exe-windows-x86_64.exe" -OutFile "$dest\\$exe.exe"`,
      "}",
    ].join("\n"),
  } as const;
}

/** Build-from-source commands per OS. Identical structure across
 *  platforms — the only difference is the executable name on Windows
 *  (`.exe`). FreePascal must already be installed. */
const SOURCE_COMMANDS = {
  macos:
    "git clone https://github.com/frostney/GocciaScript\ncd GocciaScript\n./build.pas loader testrunner repl\n./build/GocciaScriptLoader --help",
  linux:
    "git clone https://github.com/frostney/GocciaScript\ncd GocciaScript\n./build.pas loader testrunner repl\n./build/GocciaScriptLoader --help",
  windows:
    "git clone https://github.com/frostney/GocciaScript\ncd GocciaScript\n./build.pas loader testrunner repl\n.\\build\\GocciaScriptLoader.exe --help",
} as const;

export function Install({
  release,
  locale,
}: {
  release: ReleaseInfo | null;
  /** BCP-47 locale tag resolved from `Accept-Language` server-side.
   *  Used to format the release date so SSR matches the browser. */
  locale: string;
}) {
  // Arch auto-detect for the Pre-built binaries section. SSR renders
  // the "arm64" default; the client useEffect overrides on first render
  // based on the actual platform. We render BOTH archs in the command
  // — only the comment markers shift — so the SSR/client transition
  // is visually subtle.
  const [arch, setArch] = useState<ArchKey>("arm64");
  useEffect(() => {
    if (typeof navigator === "undefined") return;
    setArch(detectArch(navigator.userAgent));
  }, []);
  const prebuilt = buildPrebuiltCommands(arch);

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
          </div>

          <div className="install-hero-meta">
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
              <LatestVersion release={release} locale={locale} />
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
        </div>

        <div className="install-methods">
          {/* Featured first method — the recommended path for most users. */}
          <section
            id="quick"
            className="install-method install-method-featured"
          >
            <AnchorH2 id="quick">Quick install — one-liner</AnchorH2>
            <p>
              The fastest path: fetch the right binary for your platform and
              drop it on your <code>$PATH</code>. We&apos;ve preselected your
              detected OS — pick a different tab if needed.
            </p>
            <QuickInstall />
          </section>

          {/* Native package managers — one consolidated method, OS-tabbed. */}
          <section id="package-manager" className="install-method">
            <AnchorH2 id="package-manager">Native package manager</AnchorH2>
            <p>
              Through your system&apos;s package manager — Homebrew on macOS,
              apt on Debian/Ubuntu, Scoop on Windows. Stays on the latest
              stable.
            </p>
            <CommandTabs
              tabs={OS_TABS}
              storageKey="goccia.install.os"
              commands={SYSTEM_PM_COMMANDS}
            />
          </section>

          {/* Run without installing — JS-package-manager tabbed. */}
          <section id="npx" className="install-method">
            <AnchorH2 id="npx">Run without installing</AnchorH2>
            <p>
              For one-off scripts and CI tasks where you don&apos;t want a
              global install. The package is fetched into the manager&apos;s
              cache on first run; subsequent invocations are essentially free.
            </p>
            <CommandTabs
              tabs={PM_TABS}
              storageKey="goccia.install.pm"
              commands={{
                npm: "npx gocciascript script.js",
                bun: "bunx gocciascript script.js",
                pnpm: "pnpm dlx gocciascript script.js",
                yarn: "yarn dlx gocciascript script.js",
                deno: "deno run -A npm:gocciascript script.js",
              }}
            />
          </section>

          {/* Global install via JS package managers. */}
          <section id="npm" className="install-method">
            <AnchorH2 id="npm">JavaScript package managers</AnchorH2>
            <p>
              Cross-platform global install via npm, Bun, pnpm, Yarn, or Deno.
              The npm package downloads the right native binary for your OS /
              arch on install — same pattern as esbuild and biome.
            </p>
            <CommandTabs
              tabs={PM_TABS}
              storageKey="goccia.install.pm"
              commands={{
                npm: "npm install -g gocciascript",
                bun: "bun install -g gocciascript",
                pnpm: "pnpm add -g gocciascript",
                yarn: "yarn global add gocciascript",
                deno: "deno install -gA -n gocciascript npm:gocciascript",
              }}
            />
          </section>

          {/* Pre-built binaries — OS-tabbed, downloads loader + testrunner + REPL. */}
          <section id="binaries" className="install-method">
            <AnchorH2 id="binaries">Pre-built binaries</AnchorH2>
            <p>
              Download the binaries for your OS / arch from the GitHub release
              and drop them on your <code>$PATH</code>. Includes{" "}
              <code>GocciaScriptLoader</code> (the runtime),{" "}
              <code>GocciaTestRunner</code>, and <code>GocciaREPL</code>.
            </p>
            <CommandTabs
              tabs={OS_TABS}
              storageKey="goccia.install.os"
              commands={prebuilt}
            />
          </section>

          {/* Build from source — OS-tabbed (Windows uses .exe). */}
          <section id="from-source" className="install-method">
            <AnchorH2 id="from-source">Build from source</AnchorH2>
            <p>
              <a
                href="https://www.freepascal.org"
                target="_blank"
                rel="noopener noreferrer"
              >
                FreePascal compiler
              </a>{" "}
              required. Clone, run the build script, then point your{" "}
              <code>$PATH</code> at <code>./build</code>.
            </p>
            <CommandTabs
              tabs={OS_TABS}
              storageKey="goccia.install.os"
              commands={SOURCE_COMMANDS}
            />
          </section>
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
