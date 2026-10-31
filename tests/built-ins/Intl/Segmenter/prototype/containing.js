/*---
description: >
  Segments.prototype.containing returns the segment data object at an index,
  and leaves the realm's segment iterator prototype usable when it runs
  before any segment iterator exists.
features: [Intl, Intl.Segmenter]
---*/

const isIntl = typeof Intl !== "undefined";

// These tests run before anything in this file makes a segment iterator, so
// containing() is the first to touch the segment iterator prototype. The
// objects passed to expect() are snapshotted at file idle by the test
// runner's heap-snapshot mode, which reads that prototype's method host.
describe.runIf(isIntl && typeof Intl.Segmenter !== "undefined")("Segments.prototype.containing", () => {
  test("returns the grapheme segment at an index", () => {
    const data = new Intl.Segmenter("en", { granularity: "grapheme" }).segment("ab").containing(0);
    expect(data).toEqual({ segment: "a", index: 0, input: "ab" });
  });

  test("returns the word segment enclosing an index, with isWordLike", () => {
    const data = new Intl.Segmenter("en", { granularity: "word" }).segment("Hello world").containing(8);
    expect(data).toEqual({ segment: "world", index: 6, input: "Hello world", isWordLike: true });
  });

  test("returns undefined past the end of the string", () => {
    expect(new Intl.Segmenter().segment("ab").containing(2)).toBeUndefined();
  });

  test("segment iterators made afterwards iterate", () => {
    const segments = new Intl.Segmenter("en", { granularity: "grapheme" }).segment("xy");
    const iterator = segments[Symbol.iterator]();
    expect(iterator.next()).toEqual({ value: { segment: "x", index: 0, input: "xy" }, done: false });
    expect(Array.from(segments).map((s) => s.segment)).toEqual(["x", "y"]);
  });
});
