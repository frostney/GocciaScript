import { parseSemverTag } from "@/lib/github";
import { CANONICAL_SITE_URL } from "@/lib/site-url";

// Resolved against the canonical URL rather than appended to it: the canonical
// URL ends in '/', and appending a second one printed `…dev//install`.
const installScriptUrl = (path: string) =>
  new URL(path, CANONICAL_SITE_URL).href;

const UNIX_INSTALL_COMMAND = `curl -fsSL ${installScriptUrl("/install")} | sh`;

export const QUICK_INSTALL_COMMANDS = {
  macos: UNIX_INSTALL_COMMAND,
  linux: UNIX_INSTALL_COMMAND,
  windows: `irm ${installScriptUrl("/install.ps1")} | iex`,
} as const;

export const HOMEBREW_INSTALL_COMMAND =
  "brew install frostney/tap/gocciascript";

/** The runner binary a release archive carries. 0.14 renamed
 *  `GocciaScriptLoader` to `GocciaRunner`; the install page renders
 *  commands for the latest release, which may still predate the rename.
 *  Tags that are not semver (the rolling `nightly`) and the `v0.0.0`
 *  placeholder use the current name. */
export function runnerBinaryForRelease(tag: string): string {
  const semver = parseSemverTag(tag);
  if (semver && semver.major === 0 && semver.minor > 0 && semver.minor < 14) {
    return "GocciaScriptLoader";
  }
  return "GocciaRunner";
}
