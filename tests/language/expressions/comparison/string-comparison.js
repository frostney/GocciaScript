/*---
description: String comparison operators with different lengths, boundary sizes, and unicode
features: [strict-equality-operator, relational-operators]
---*/

describe("string equality with different lengths", () => {
  test("empty string comparisons", () => {
    const a = "";
    const b = "";
    expect(a === b).toBe(true);
    expect(a !== b).toBe(false);
    expect(a === "a").toBe(false);
    expect(a !== "a").toBe(true);
    expect("a" === a).toBe(false);
  });

  test("same content same length", () => {
    const a = "abc";
    const b = "abc";
    const c = "hello world";
    const d = "hello world";
    expect(a === b).toBe(true);
    expect(c === d).toBe(true);
    expect(a !== b).toBe(false);
  });

  test("same length different content", () => {
    expect("abc" === "abd").toBe(false);
    expect("abc" !== "abd").toBe(true);
    expect("aaa" === "aab").toBe(false);
    expect("xyz" === "xyw").toBe(false);
  });

  test("different lengths same prefix", () => {
    expect("a" === "ab").toBe(false);
    expect("ab" === "a").toBe(false);
    expect("abc" === "abcd").toBe(false);
    expect("abcd" === "abc").toBe(false);
    expect("hello" === "hello!").toBe(false);
  });
});

describe("string relational operators with varying lengths", () => {
  test("empty string is less than any non-empty string", () => {
    const empty = "";
    const alsoEmpty = "";
    expect(empty < "a").toBe(true);
    expect(empty < "z").toBe(true);
    expect(empty < "abc").toBe(true);
    expect("a" > empty).toBe(true);
    expect(empty <= alsoEmpty).toBe(true);
    expect(empty >= alsoEmpty).toBe(true);
  });

  test("shorter prefix is less than longer string", () => {
    expect("a" < "ab").toBe(true);
    expect("ab" < "abc").toBe(true);
    expect("abc" < "abcd").toBe(true);
    expect("ab" > "a").toBe(true);
    expect("abc" > "ab").toBe(true);
  });

  test("lexicographic ordering same length", () => {
    const a = "abc";
    const b = "abc";
    expect(a < "abd").toBe(true);
    expect("abd" > a).toBe(true);
    expect("aaa" < "aab").toBe(true);
    expect("zzz" > "zzy").toBe(true);
    expect(a <= b).toBe(true);
    expect(a >= b).toBe(true);
    expect(a <= "abd").toBe(true);
    expect("abd" >= a).toBe(true);
  });

  test("lexicographic ordering different lengths", () => {
    expect("b" > "aa").toBe(true);
    expect("z" > "abc").toBe(true);
    expect("aa" < "b").toBe(true);
    expect("apple" < "banana").toBe(true);
    expect("banana" > "apple").toBe(true);
  });
});

describe("short strings (1-5 chars)", () => {
  test("single character comparisons", () => {
    const a = "a";
    const b = "a";
    expect(a === b).toBe(true);
    expect(a < "b").toBe(true);
    expect("z" > a).toBe(true);
    expect("A" < a).toBe(true);
    expect("Z" < a).toBe(true);
  });

  test("2-5 character strings", () => {
    const ab1 = "ab";
    const ab2 = "ab";
    const hello1 = "hello";
    const hello2 = "hello";
    expect(ab1 === ab2).toBe(true);
    expect(ab1 < "ac").toBe(true);
    expect(hello1 === hello2).toBe(true);
    expect(hello1 < "world").toBe(true);
    expect("abcde" > "abcdd").toBe(true);
  });
});

describe("medium length strings", () => {
  test("6-char strings", () => {
    const a = "abcdef";
    const b = "abcdef";
    const c = "abcdeg";
    expect(a === b).toBe(true);
    expect(a < c).toBe(true);
    expect(c > a).toBe(true);
  });

  test("10-char strings", () => {
    const a = "abcdefghij";
    const b = "abcdefghij";
    const c = "abcdefghik";
    expect(a === b).toBe(true);
    expect(a < c).toBe(true);
    expect(a !== c).toBe(true);
  });

  test("13-char strings", () => {
    const a = "abcdefghijklm";
    const b = "abcdefghijklm";
    const c = "abcdefghijkln";
    expect(a === b).toBe(true);
    expect(a < c).toBe(true);
    expect(c > a).toBe(true);
    expect(a !== c).toBe(true);
  });
});

describe("long strings (14+ chars)", () => {
  test("14-char strings", () => {
    const a = "abcdefghijklmn";
    const b = "abcdefghijklmn";
    const c = "abcdefghijklmo";
    expect(a === b).toBe(true);
    expect(a < c).toBe(true);
    expect(c > a).toBe(true);
    expect(a !== c).toBe(true);
  });

  test("15-char strings", () => {
    const a = "abcdefghijklmno";
    const b = "abcdefghijklmno";
    const c = "abcdefghijklmnp";
    expect(a === b).toBe(true);
    expect(a < c).toBe(true);
    expect(c > b).toBe(true);
  });

  test("20-char strings", () => {
    const a = "abcdefghijklmnopqrst";
    const b = "abcdefghijklmnopqrst";
    const c = "abcdefghijklmnopqrsu";
    expect(a === b).toBe(true);
    expect(a < c).toBe(true);
    expect(c > a).toBe(true);
  });

  test("long strings with shared prefix", () => {
    const prefix = "shared_prefix__";
    const a = prefix + "alpha";
    const a2 = prefix + "alpha";
    const b = prefix + "beta";
    expect(a < b).toBe(true);
    expect(b > a).toBe(true);
    expect(a === a2).toBe(true);
  });
});

describe("short vs long string comparisons", () => {
  test("short vs long string equality", () => {
    expect("abc" === "abcdefghijklmn").toBe(false);
    expect("abcdefghijklmn" === "abc").toBe(false);
  });

  test("short vs long string ordering", () => {
    expect("abc" < "abcdefghijklmn").toBe(true);
    expect("abcdefghijklmn" > "abc").toBe(true);
    expect("z" > "abcdefghijklmn").toBe(true);
    expect("abcdefghijklmn" < "z").toBe(true);
  });

  test("13-char vs 14-char strings", () => {
    const shorter = "abcdefghijklm";
    const longer = "abcdefghijklmn";
    expect(shorter === longer).toBe(false);
    expect(shorter < longer).toBe(true);
    expect(longer > shorter).toBe(true);
    expect(shorter !== longer).toBe(true);
  });
});

describe("unicode string comparison", () => {
  test("BMP characters (accented letters)", () => {
    const a = "café";
    const b = "café";
    expect(a === b).toBe(true);
    expect(a !== "cafe").toBe(true);
    expect(a === "cafe").toBe(false);
  });

  test("emoji strings", () => {
    const a = "😀";
    const b = "😀";
    expect(a === b).toBe(true);
    expect(a !== "😁").toBe(true);
    expect(a === "😁").toBe(false);
  });

  test("emoji string ordering", () => {
    expect("😀" < "😁").toBe(true);
    expect("😁" > "😀").toBe(true);
  });

  test("multi-emoji strings", () => {
    const a = "😀😁";
    const b = "😀😁";
    expect(a === b).toBe(true);
    expect(a !== "😀😂").toBe(true);
    expect(a < "😀😂").toBe(true);
  });

  test("CJK characters", () => {
    const a = "你好";
    const b = "你好";
    expect(a === b).toBe(true);
    expect(a !== "世界").toBe(true);
    expect(a === "世界").toBe(false);
  });

  test("mixed ASCII and unicode", () => {
    const a = "hello😀";
    const b = "hello😀";
    expect(a === b).toBe(true);
    expect(a !== "hello😁").toBe(true);
    expect("abc" < "abé").toBe(true);
    expect("abé" > "abc").toBe(true);
  });

  test("unicode escape sequences", () => {
    expect("\u00e9" === "é").toBe(true);
    expect("\u0041" === "A").toBe(true);
    expect("\u{1F600}" === "😀").toBe(true);
  });
});

describe("unicode strings of varying lengths", () => {
  test("short unicode strings", () => {
    const shortUnicode = "héllo wörld";
    expect(shortUnicode === "héllo wörld").toBe(true);
  });

  test("long unicode strings", () => {
    const a = "café résumé naïve";
    const b = "café résumé naïve";
    expect(a === b).toBe(true);
    expect(a !== b).toBe(false);
  });

  test("emoji-heavy strings", () => {
    const a = "🌍🌎🌏🌐";
    const b = "🌍🌎🌏🌐";
    const c = "🌍🌎🌏🌑";
    expect(a === b).toBe(true);
    expect(a !== c).toBe(true);
    expect(a < c).toBe(true);
  });
});

describe("string comparison edge cases", () => {
  test("case sensitivity", () => {
    expect("ABC" === "abc").toBe(false);
    expect("ABC" < "abc").toBe(true);
    expect("abc" > "ABC").toBe(true);
    expect("A" < "a").toBe(true);
  });

  test("whitespace differences", () => {
    expect("hello " === "hello").toBe(false);
    expect(" hello" === "hello").toBe(false);
    expect("hello " > "hello").toBe(true);
    expect(" " < "a").toBe(true);
  });

  test("numeric strings ordered lexicographically", () => {
    expect("9" > "10").toBe(true);
    expect("2" > "10").toBe(true);
    expect("100" < "2").toBe(true);
    expect("09" < "9").toBe(true);
  });

  test("special characters", () => {
    expect("\t" < " ").toBe(true);
    expect("\n" < " ").toBe(true);
    expect("!" < "A").toBe(true);
    expect("~" > "z").toBe(true);
  });
});
