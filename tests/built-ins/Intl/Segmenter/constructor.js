/*---
description: Intl.Segmenter constructor
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl && typeof Intl.Segmenter !== "undefined")("Intl.Segmenter constructor", () => {
  test("creates an instance with a locale argument", () => {
    const seg = new Intl.Segmenter("en");
    expect(seg).toBeInstanceOf(Intl.Segmenter);
  });

  test("creates an instance with no arguments", () => {
    const seg = new Intl.Segmenter();
    expect(seg).toBeInstanceOf(Intl.Segmenter);
  });

  test("segment property is a function", () => {
    const seg = new Intl.Segmenter("en");
    expect(typeof seg.segment).toBe("function");
  });

  test("resolvedOptions returns an object with locale and granularity", () => {
    const seg = new Intl.Segmenter("en");
    const options = seg.resolvedOptions();
    expect(typeof options.locale).toBe("string");
    expect(typeof options.granularity).toBe("string");
  });

  test("creates an instance with granularity 'word'", () => {
    const seg = new Intl.Segmenter("en", { granularity: "word" });
    const options = seg.resolvedOptions();
    expect(options.granularity).toBe("word");
  });
});
