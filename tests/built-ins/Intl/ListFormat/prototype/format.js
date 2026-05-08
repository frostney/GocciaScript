/*---
description: Intl.ListFormat.prototype.format
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl && typeof Intl.ListFormat !== "undefined")("Intl.ListFormat.prototype.format", () => {
  test("formats a three-element list with conjunction", () => {
    const lf = new Intl.ListFormat("en", { style: "long", type: "conjunction" });
    const result = lf.format(["a", "b", "c"]);
    expect(result).toBe("a, b, and c");
  });

  test("formats a two-element list with conjunction", () => {
    const lf = new Intl.ListFormat("en", { style: "long", type: "conjunction" });
    const result = lf.format(["a", "b"]);
    expect(result).toBe("a and b");
  });

  test("formats a single-element list", () => {
    const lf = new Intl.ListFormat("en", { style: "long", type: "conjunction" });
    const result = lf.format(["a"]);
    expect(result).toBe("a");
  });

  test("formats an empty list as empty string", () => {
    const lf = new Intl.ListFormat("en");
    const result = lf.format([]);
    expect(result).toBe("");
  });

  test("format returns a string", () => {
    const lf = new Intl.ListFormat("en");
    expect(typeof lf.format(["x", "y"])).toBe("string");
  });
});
