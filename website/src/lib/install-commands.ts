import { CANONICAL_SITE_URL } from "@/lib/site-url";

const UNIX_INSTALL_COMMAND = `curl -fsSL ${CANONICAL_SITE_URL}/install | sh`;

export const QUICK_INSTALL_COMMANDS = {
  macos: UNIX_INSTALL_COMMAND,
  linux: UNIX_INSTALL_COMMAND,
  windows: `irm ${CANONICAL_SITE_URL}/install.ps1 | iex`,
} as const;

export const HOMEBREW_INSTALL_COMMAND =
  "brew install frostney/tap/gocciascript";
