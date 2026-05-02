/*---
description: String prototype methods use UTF-16 code unit indexes
features: [String.prototype]
---*/

describe("String.prototype UTF-16 code unit indexing", () => {
  const face = "\uD83D\uDE00";
  const text = "a" + face + "b";

  test("slice and substring operate on UTF-16 code units", () => {
    expect(text.slice(1, 3)).toBe(face);
    expect(text.substring(1, 3)).toBe(face);
    expect(text.slice(1, 2).charCodeAt(0)).toBe(0xd83d);
    expect(text.substring(2, 3).charCodeAt(0)).toBe(0xde00);
  });

  test("search methods return and consume UTF-16 code unit positions", () => {
    expect(text.indexOf(face)).toBe(1);
    expect(text.indexOf("b")).toBe(3);
    expect((face + "x" + face).lastIndexOf(face)).toBe(3);
    expect(text.includes(face, 2)).toBe(false);
    expect(text.startsWith(face, 1)).toBe(true);
    expect(text.endsWith(face, 3)).toBe(true);
  });

  test("padding computes target lengths in UTF-16 code units", () => {
    expect("x".padStart(3, face)).toBe(face + "x");
    expect("x".padEnd(3, face)).toBe("x" + face);
  });

  test("split uses UTF-16 code units for empty and non-empty separators", () => {
    const units = face.split("");
    expect(units.length).toBe(2);
    expect(units[0].charCodeAt(0)).toBe(0xd83d);
    expect(units[1].charCodeAt(0)).toBe(0xde00);
    expect(("a" + face + "b" + face + "c").split(face)).toEqual([
      "a",
      "b",
      "c",
    ]);
  });
});
