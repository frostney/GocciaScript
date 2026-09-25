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
