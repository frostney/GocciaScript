/*---
description: Intl.Segmenter.prototype.segment
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl && typeof Intl.Segmenter !== "undefined")("Intl.Segmenter.prototype.segment", () => {
  test("segment returns an iterable", () => {
    const seg = new Intl.Segmenter("en", { granularity: "grapheme" });
    const segments = seg.segment("Hello");
    expect(typeof segments[Symbol.iterator]).toBe("function");
  });

  test("iterating grapheme segments of 'Hello' yields five segments", () => {
    const seg = new Intl.Segmenter("en", { granularity: "grapheme" });
    const segments = Array.from(seg.segment("Hello"));
    expect(segments.length).toBe(5);
  });

  test("each segment has a 'segment' string property", () => {
    const seg = new Intl.Segmenter("en", { granularity: "grapheme" });
    const segments = Array.from(seg.segment("Hi"));
    expect(segments[0].segment).toBe("H");
    expect(segments[1].segment).toBe("i");
  });

  test("word granularity segments contain isWordLike property", () => {
    const seg = new Intl.Segmenter("en", { granularity: "word" });
    const segments = Array.from(seg.segment("Hello world"));
    const words = segments.filter(s => s.isWordLike);
    expect(words.length).toBe(2);
  });
});

describe("Segments.prototype.containing non-finite index", () => {
  test("Infinity returns undefined", () => {
    expect(new Intl.Segmenter().segment("ab").containing(Infinity)).toBeUndefined();
  });

  test("-Infinity returns undefined", () => {
    expect(new Intl.Segmenter().segment("ab").containing(-Infinity)).toBeUndefined();
  });
});
