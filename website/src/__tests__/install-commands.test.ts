import { describe, expect, test } from "bun:test";
import {
  HOMEBREW_INSTALL_COMMAND,
  QUICK_INSTALL_COMMANDS,
} from "@/lib/install-commands";

describe("install commands", () => {
  test("keeps the install script as the default on Unix platforms", () => {
    expect(QUICK_INSTALL_COMMANDS.macos).toBe(
      "curl -fsSL https://www.gocciascript.dev/install | sh",
    );
    expect(QUICK_INSTALL_COMMANDS.linux).toBe(
      "curl -fsSL https://www.gocciascript.dev/install | sh",
    );
  });

  test("points Windows at the PowerShell install script", () => {
    expect(QUICK_INSTALL_COMMANDS.windows).toBe(
      "irm https://www.gocciascript.dev/install.ps1 | iex",
    );
  });

  test("exposes the maintained Homebrew tap as a separate option", () => {
    expect(HOMEBREW_INSTALL_COMMAND).toBe(
      "brew install frostney/tap/gocciascript",
    );
  });
});
