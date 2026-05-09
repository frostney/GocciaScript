/*---
description: Intl.Segmenter.prototype.segment
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";
const hasICU = isIntl && new Intl.NumberFormat("en").format(1000).includes(",");

describe.runIf(isIntl && typeof Intl.Segmenter !== "undefined")("Intl.Segmenter.prototype.segment", () => {
  test("segment returns an iterable", () => {
    const seg = new Intl.Segmenter("en", { granularity: "grapheme" });
    const segments = seg.segment("Hello");
    expect(typeof segments[Symbol.iterator]).toBe("function");
  });

  test.runIf(hasICU)("iterating grapheme segments of 'Hello' yields five segments", () => {
    const seg = new Intl.Segmenter("en", { granularity: "grapheme" });
    const segments = Array.from(seg.segment("Hello"));
    expect(segments.length).toBe(5);
  });

  test.runIf(hasICU)("each segment has a 'segment' string property", () => {
    const seg = new Intl.Segmenter("en", { granularity: "grapheme" });
    const segments = Array.from(seg.segment("Hi"));
    expect(segments[0].segment).toBe("H");
    expect(segments[1].segment).toBe("i");
  });

  test.runIf(hasICU)("word granularity segments contain isWordLike property", () => {
    const seg = new Intl.Segmenter("en", { granularity: "word" });
    const segments = Array.from(seg.segment("Hello world"));
    const words = segments.filter(s => s.isWordLike);
    expect(words.length).toBe(2);
  });
});
