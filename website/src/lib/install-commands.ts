import { CANONICAL_SITE_URL } from "@/lib/site-url";

export const QUICK_INSTALL_COMMANDS = {
  macos: "brew install frostney/tap/gocciascript",
  linux: `curl -fsSL ${CANONICAL_SITE_URL}/install | sh`,
  windows: `irm ${CANONICAL_SITE_URL}/install.ps1 | iex`,
} as const;
