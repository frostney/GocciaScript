import { describe, expect, test } from "bun:test";
import {
  HOMEBREW_INSTALL_COMMAND,
  QUICK_INSTALL_COMMANDS,
  runnerBinaryForRelease,
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

  test("names the runner each release archive carries", () => {
    expect(runnerBinaryForRelease("v0.13.2")).toBe("GocciaScriptLoader");
    expect(runnerBinaryForRelease("0.7.0")).toBe("GocciaScriptLoader");
    expect(runnerBinaryForRelease("v0.14.0")).toBe("GocciaRunner");
    expect(runnerBinaryForRelease("v1.0.0")).toBe("GocciaRunner");
    expect(runnerBinaryForRelease("nightly")).toBe("GocciaRunner");
    expect(runnerBinaryForRelease("v0.0.0")).toBe("GocciaRunner");
  });
});
