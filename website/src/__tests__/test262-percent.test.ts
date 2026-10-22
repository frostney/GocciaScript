import { describe, expect, test } from "bun:test";
import { compatibilityPercent } from "@/lib/test262-percent";

describe("compatibilityPercent", () => {
  test("formats report counts to one decimal with the markdown rounding", () => {
    expect(compatibilityPercent(2, 3)).toBe("66.7%");
    expect(compatibilityPercent(1, 6)).toBe("16.7%");
    expect(compatibilityPercent(1, 8)).toBe("12.5%");
    expect(compatibilityPercent(3, 3)).toBe("100.0%");
    expect(compatibilityPercent(0, 3)).toBe("0.0%");
  });

  test("handles empty report counts without dividing by zero", () => {
    expect(compatibilityPercent(0, 0)).toBe("0.0%");
    expect(compatibilityPercent(0, -1)).toBe("0.0%");
  });
});
