/*---
description: >
  A compiled RegExp pattern is freed once no RegExp object refers to it, and
  its charge against the memory limit with it. Compiling 40,000 distinct
  patterns and dropping each, about twice what the 32 MiB limit holds when
  none is freed, completes without a refusal and leaves room for an 8 MiB
  buffer. A RegExp still in use keeps its pattern through the collections.
features: [Goccia.gc]
---*/

const indices = (n) => Array.from({ length: n }, (_, i) => i);

describe("compiled RegExp patterns under the memory limit", () => {
  test("are freed with the last RegExp that uses them", () => {
    let last = null;
    for (const i of indices(40000)) last = new RegExp(`(?:x${i}|y${i})+[a-z]{2,${(i % 50) + 3}}z`);
    expect(last.test("x39999y39999abz")).toBe(true);
    expect(new ArrayBuffer(8 * 1024 * 1024).byteLength).toBe(8 * 1024 * 1024);
  });

  test("stay with a RegExp still in use through collections", () => {
    const kept = new RegExp("k(e+)pt", "g");
    for (const i of indices(2000)) new RegExp(`dropped${i}`);
    Goccia.gc();
    expect("keeept keept".match(kept)).toEqual(["keeept", "keept"]);
    expect(kept.exec("xkeept")[1]).toBe("ee");
  });

  test("of a recompiled RegExp are its new ones", () => {
    const re = /old/;
    re.compile("ne(w)");
    for (const i of indices(2000)) new RegExp(`dropped${i}`);
    Goccia.gc();
    expect(re.exec("brand new")[1]).toBe("w");
    expect(re.test("old")).toBe(false);
  });

  test("of string patterns, which no RegExp keeps", () => {
    for (const i of indices(2000)) "abc".search(`b${i}`);
    Goccia.gc();
    expect("xyzabc".search("abc")).toBe(3);
    expect("xyzabc".match("z(a)")[1]).toBe("a");
  });
});
