import { describe, expect, test } from "bun:test";
import { QUICK_INSTALL_COMMANDS } from "@/lib/install-commands";

describe("quick install commands", () => {
  test("uses the maintained Homebrew tap on macOS", () => {
    expect(QUICK_INSTALL_COMMANDS.macos).toBe(
      "brew install frostney/tap/gocciascript",
    );
  });
});
