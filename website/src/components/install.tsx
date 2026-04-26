"use client";

import Link from "next/link";
import { useEffect, useState } from "react";
import { AnchorH2 } from "@/components/anchor-heading";
import {
  type ArchKey,
  CommandTabs,
  detectArch,
  OS_TABS,
  type OsKey,
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

/** Once the runtime is mature enough to publish through native package
 *  managers and the JS package managers, flip this to `true` to reveal
 *  three sections in the methods list:
 *    - Native package manager (Homebrew / apt / Scoop)
 *    - Run without installing  (npx / bunx / pnpm dlx / yarn dlx / deno run)
 *    - JavaScript package managers (npm / Bun / pnpm / Yarn / Deno)
 *  The data + JSX for each is preserved below behind this gate. */
const SHOW_PACKAGE_MANAGER_SECTIONS = false;

/** Native package-manager commands per OS. APT and Homebrew are
 *  intentionally absent — the runtime isn't mature enough to publish
 *  through `brew` or `apt` repositories yet (no signed Debian repo
 *  yet, no Homebrew tap repo yet). Reintroduce both when the
 *  publishing pipeline lands; the docs in `packaging/{apt,homebrew}/`
 *  describe what each entry should be. */
const SYSTEM_PM_COMMANDS = {
  macos: "# Homebrew tap is coming once the runtime stabilises.",
  linux: "# APT repository is coming once the runtime stabilises.",
  windows:
    "scoop bucket add frostney https://github.com/frostney/scoop-bucket\nscoop install gocciascript",
} as const;

/** Pre-built binary download instructions per OS.
 *
 *  Releases ship a single archive per OS / arch with the full
 *  toolchain inside (loader / test runner / REPL under `build/`):
 *    gocciascript-{version}-macos-{arm64|x64}.zip
 *    gocciascript-{version}-linux-{arm64|x64}.tar.gz
 *    gocciascript-{version}-windows-{x64|x86}.zip
 *
 *  The macOS / Linux variants render BOTH arch blocks, with the
 *  detected arch active and the other commented out so the user can
 *  swap by toggling comment markers. Windows ships two archs; we
 *  show the detected one and an x86 fallback for older machines. */
const ARCH_LABELS = {
  macos: { arm64: "Apple Silicon (arm64)", x64: "Intel (x64)", x86: "" },
  linux: { arm64: "ARM (arm64)", x64: "x64", x86: "" },
  windows: { arm64: "", x64: "x64", x86: "32-bit (x86)" },
} as const;

/** Strip the optional `v` prefix from a tag (`v0.6.1` → `0.6.1`).
 *  Release artifact filenames don't include the prefix. */
function stripV(tag: string): string {
  return tag.replace(/^v/, "");
}

function archiveFilename(os: OsKey, arch: ArchKey, version: string): string {
  const ext = os === "linux" ? "tar.gz" : "zip";
  return `gocciascript-${version}-${os}-${arch}.${ext}`;
}

function archiveUrl(os: OsKey, arch: ArchKey, tag: string): string {
  return `https://github.com/frostney/GocciaScript/releases/download/${tag}/${archiveFilename(os, arch, stripV(tag))}`;
}

/** Build a Unix prebuilt block for a given OS at a given arch. Active
 *  arch lines are uncommented; alternate arch is shown fully commented
 *  underneath. */
function unixPrebuiltBlock(
  os: "macos" | "linux",
  active: ArchKey,
  commented: boolean,
  tag: string,
): string {
  const url = archiveUrl(os, active, tag);
  const archive = archiveFilename(os, active, stripV(tag));
  const label = ARCH_LABELS[os][active];
  const c = commented ? "# " : "";
  const note = commented ? "uncomment to use instead" : "auto-detected";
  const unpack =
    os === "linux" ? `${c}tar xzf "${archive}"` : `${c}unzip -q "${archive}"`;
  return [
    `# ${label} — ${note}`,
    `${c}curl -fsSL -O "${url}"`,
    unpack,
    `${c}chmod +x build/GocciaScriptLoader build/GocciaTestRunner build/GocciaREPL`,
    `${c}sudo mv build/GocciaScriptLoader build/GocciaTestRunner build/GocciaREPL /usr/local/bin/`,
  ].join("\n");
}

/** Build a Windows prebuilt block (PowerShell). Active arch lines are
 *  uncommented; alternate arch is fully commented. */
function windowsPrebuiltBlock(
  active: ArchKey,
  commented: boolean,
  tag: string,
): string {
  const url = archiveUrl("windows", active, tag);
  const archive = archiveFilename("windows", active, stripV(tag));
  const label = ARCH_LABELS.windows[active];
  const c = commented ? "# " : "";
  const note = commented ? "uncomment to use instead" : "auto-detected";
  return [
    `# ${label} — ${note}`,
    `${c}Invoke-WebRequest -Uri "${url}" -OutFile "${archive}"`,
    `${c}Expand-Archive -Path "${archive}" -DestinationPath . -Force`,
    `${c}New-Item -ItemType Directory -Force -Path "$env:USERPROFILE\\bin" | Out-Null`,
    `${c}Move-Item -Force build\\GocciaScriptLoader.exe, build\\GocciaTestRunner.exe, build\\GocciaREPL.exe "$env:USERPROFILE\\bin\\"`,
  ].join("\n");
}

function buildPrebuiltCommands(arch: ArchKey, tag: string) {
  // macOS / Linux: arm64 ↔ x64 swap. Windows: x64 ↔ x86 swap.
  const macosOther: ArchKey = arch === "arm64" ? "x64" : "arm64";
  const linuxOther: ArchKey = arch === "arm64" ? "x64" : "arm64";
  const winActive: ArchKey = arch === "x86" ? "x86" : "x64";
  const winOther: ArchKey = winActive === "x64" ? "x86" : "x64";
  // macOS / Linux can't be x86 in our matrix — clamp to x64 if the
  // detector returned x86 (it shouldn't, but keep the type happy).
  const macosActive: ArchKey = arch === "x86" ? "x64" : arch;
  const linuxActive: ArchKey = arch === "x86" ? "x64" : arch;
  return {
    macos: [
      unixPrebuiltBlock("macos", macosActive, false, tag),
      "",
      unixPrebuiltBlock("macos", macosOther, true, tag),
    ].join("\n"),
    linux: [
      unixPrebuiltBlock("linux", linuxActive, false, tag),
      "",
      unixPrebuiltBlock("linux", linuxOther, true, tag),
    ].join("\n"),
    windows: [
      windowsPrebuiltBlock(winActive, false, tag),
      "",
      windowsPrebuiltBlock(winOther, true, tag),
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
  // Use the resolved release tag if we have one, otherwise leave a
  // visible placeholder so the user knows to substitute.
  const tag = release?.tagName ?? "v0.0.0";
  const prebuilt = buildPrebuiltCommands(arch, tag);

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
            <p className="install-method-aux">
              <a
                href="/install"
                target="_blank"
                rel="noopener noreferrer"
                className="install-link"
              >
                View install script
              </a>{" "}
              before piping it into your shell. Want unreleased fixes? Swap{" "}
              <code>/install</code> for <code>/install-nightly</code> to track
              the rolling nightly channel.
            </p>
          </section>

          {/* The three package-manager-flavoured sections below are
              gated behind SHOW_PACKAGE_MANAGER_SECTIONS — hidden until
              the runtime ships through `brew` / `apt` / `npm` for real.
              The data and JSX are preserved so unhiding is a one-flag
              flip, not a re-build. */}
          {SHOW_PACKAGE_MANAGER_SECTIONS && (
            <>
              {/* Native package managers — one consolidated method, OS-tabbed. */}
              <section id="package-manager" className="install-method">
                <AnchorH2 id="package-manager">Native package manager</AnchorH2>
                <p>
                  Through your system&apos;s package manager — Homebrew on
                  macOS, apt on Debian/Ubuntu, Scoop on Windows. Stays on the
                  latest stable.
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
                  cache on first run; subsequent invocations are essentially
                  free.
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
                  Cross-platform global install via npm, Bun, pnpm, Yarn, or
                  Deno. The npm package downloads the right native binary for
                  your OS / arch on install — same pattern as esbuild and biome.
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
            </>
          )}

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
