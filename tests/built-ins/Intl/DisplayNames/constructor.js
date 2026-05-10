/*---
description: Intl.DisplayNames constructor
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl && typeof Intl.DisplayNames !== "undefined")("Intl.DisplayNames constructor", () => {
  test("creates an instance with locale and type 'region'", () => {
    const dn = new Intl.DisplayNames("en", { type: "region" });
    expect(dn).toBeInstanceOf(Intl.DisplayNames);
  });

  test("of returns a string for a valid region code", () => {
    const dn = new Intl.DisplayNames("en", { type: "region" });
    const result = dn.of("US");
    expect(typeof result).toBe("string");
    expect(result.length > 0).toBe(true);
  });

  test("creates an instance with type 'language'", () => {
    const dn = new Intl.DisplayNames("en", { type: "language" });
    expect(dn).toBeInstanceOf(Intl.DisplayNames);
  });

  test("throws TypeError when type option is missing", () => {
    expect(() => new Intl.DisplayNames("en")).toThrow(TypeError);
  });

  test("resolvedOptions returns an object with locale and type", () => {
    const dn = new Intl.DisplayNames("en", { type: "region" });
    const options = dn.resolvedOptions();
    expect(typeof options.locale).toBe("string");
    expect(options.type).toBe("region");
  });
});
