/*---
description: RegExp.prototype.exec
features: [RegExp.prototype.exec]
---*/

test("exec returns a match array with captures and metadata", () => {
  const match = /(ab)(c)/.exec("zabcq");

  expect(match[0]).toBe("abc");
  expect(match[1]).toBe("ab");
  expect(match[2]).toBe("c");
  expect(match.index).toBe(1);
  expect(match.input).toBe("zabcq");
});

test("exec returns null when there is no match", () => {
  expect(/abc/.exec("zzz")).toBe(null);
});

test("exec updates lastIndex for global regexes", () => {
  const regex = /a/g;
  const first = regex.exec("aba");

  expect(first[0]).toBe("a");
  expect(first.index).toBe(0);
  expect(regex.lastIndex).toBe(1);

  const second = regex.exec("aba");
  expect(second[0]).toBe("a");
  expect(second.index).toBe(2);
  expect(regex.lastIndex).toBe(3);

  const third = regex.exec("aba");
  expect(third).toBe(null);
  expect(regex.lastIndex).toBe(0);
});

test("exec preserves anchor semantics when lastIndex is non-zero", () => {
  const regex = /^/g;

  regex.lastIndex = 1;
  expect(regex.exec("ab")).toBe(null);
  expect(regex.lastIndex).toBe(0);
});

test("exec returns groups property with named captures", () => {
  const re = new RegExp("(?<first>\\w+) (?<last>\\w+)");
  const result = re.exec("John Doe");
  expect(result.groups.first).toBe("John");
  expect(result.groups.last).toBe("Doe");
});

test("exec returns undefined groups when no named captures", () => {
  const result = /(\w+)/.exec("hello");
  expect(result.groups).toBe(undefined);
});

test("exec populates indices when the d flag is present", () => {
  const result = /a(b)c/d.exec("abc");

  expect(result.indices[0][0]).toBe(0);
  expect(result.indices[0][1]).toBe(3);
  expect(result.indices[1][0]).toBe(1);
  expect(result.indices[1][1]).toBe(2);
  expect(result.indices.groups).toBe(undefined);
});

test("exec omits indices without the d flag", () => {
  const result = /a(b)c/.exec("abc");

  expect(result.indices).toBe(undefined);
});

test("exec indices preserve unmatched captures", () => {
  const result = /(\w\w)(\W)?/d.exec("bab");

  expect(result.indices[0][0]).toBe(0);
  expect(result.indices[0][1]).toBe(2);
  expect(result.indices[1][0]).toBe(0);
  expect(result.indices[1][1]).toBe(2);
  expect(result.indices[2]).toBe(undefined);
});

test("exec indices expose named capture groups", () => {
  const result = /(?<a>.)(?<b>.)/d.exec("ab");

  expect(result.indices.groups.a[0]).toBe(0);
  expect(result.indices.groups.a[1]).toBe(1);
  expect(result.indices.groups.b[0]).toBe(1);
  expect(result.indices.groups.b[1]).toBe(2);
  expect(Object.getPrototypeOf(result.indices.groups)).toBe(null);
});

test("exec indices support unicode escaped named capture groups", () => {
  const result = /(?<\u{03C0}>a)/du.exec("bab");

  expect(result.indices.groups.π[0]).toBe(1);
  expect(result.indices.groups.\u03C0[1]).toBe(2);
});

test("exec indices use UTF-16 code units in unicode and non-unicode modes", () => {
  const text = String.fromCodePoint(0x1d401);
  const nonUnicode = /./d.exec(text);

  expect(nonUnicode[0].length).toBe(1);
  expect(nonUnicode.indices[0][0]).toBe(0);
  expect(nonUnicode.indices[0][1]).toBe(1);

  const unicode = /./du.exec(text);
  expect(unicode[0].length).toBe(2);
  expect(unicode.indices[0][0]).toBe(0);
  expect(unicode.indices[0][1]).toBe(2);

  const literal = new RegExp(String.fromCodePoint(0x1d401), "d").exec(text);
  expect(literal[0].length).toBe(2);
  expect(literal.indices[0][0]).toBe(0);
  expect(literal.indices[0][1]).toBe(2);

  const classMatch = new RegExp("[" + String.fromCodePoint(0x1d401) + "]", "d").exec(text);
  expect(classMatch[0].length).toBe(1);
  expect(classMatch.indices[0][0]).toBe(0);
  expect(classMatch.indices[0][1]).toBe(1);
});

test("non-unicode braced unicode escape remains an identity escape", () => {
  const re = new RegExp("\\u{61}", "d");
  const result = re.exec("u".repeat(61));

  expect(re.exec("a")).toBe(null);
  expect(result[0].length).toBe(61);
  expect(result.indices[0][0]).toBe(0);
  expect(result.indices[0][1]).toBe(61);
});

test("exec with duplicate named groups — first alternative matches", () => {
  const re = new RegExp("(?<year>[0-9][0-9][0-9][0-9])-[0-9][0-9]|[0-9][0-9]-(?<year>[0-9][0-9][0-9][0-9])");
  const result = re.exec("2026-04");
  expect(result.groups.year).toBe("2026");
});

test("exec with duplicate named groups — second alternative matches", () => {
  const re = new RegExp("(?<year>[0-9][0-9][0-9][0-9])-[0-9][0-9]|[0-9][0-9]-(?<year>[0-9][0-9][0-9][0-9])");
  const result = re.exec("04-2026");
  expect(result.groups.year).toBe("2026");
});

test("exec with duplicate named groups selects the participating alternative", () => {
  const re = new RegExp("(?<x>a)|(?<x>b)");
  const result1 = re.exec("b");
  expect(result1.groups.x).toBe("b");
  const result2 = re.exec("a");
  expect(result2.groups.x).toBe("a");
});

test("exec with three alternatives sharing the same named group", () => {
  const re = new RegExp("(?<val>\\d+)|(?<val>[a-z]+)|(?<val>[A-Z]+)");
  const result1 = re.exec("123");
  expect(result1.groups.val).toBe("123");
  const result2 = re.exec("abc");
  expect(result2.groups.val).toBe("abc");
  const result3 = re.exec("ABC");
  expect(result3.groups.val).toBe("ABC");
});

test("exec with duplicate names inside non-capturing group", () => {
  const re = new RegExp("(?:(?<x>a)|(?<x>b))c");
  const result1 = re.exec("ac");
  expect(result1.groups.x).toBe("a");
  const result2 = re.exec("bc");
  expect(result2.groups.x).toBe("b");
});

test("exec with multiple sets of duplicate names", () => {
  const re = new RegExp("(?<a>x)(?<b>1)|(?<a>y)(?<b>2)");
  const result1 = re.exec("x1");
  expect(result1.groups.a).toBe("x");
  expect(result1.groups.b).toBe("1");
  const result2 = re.exec("y2");
  expect(result2.groups.a).toBe("y");
  expect(result2.groups.b).toBe("2");
});

test("exec with duplicate names and String.prototype.match", () => {
  const re = new RegExp("(?<n>\\d+)|(?<n>[a-z]+)");
  const result = "hello".match(re);
  expect(result.groups.n).toBe("hello");
});

test("exec with duplicate names and String.prototype.replace using $<name>", () => {
  const re = new RegExp("(?<val>\\d+)|(?<val>[a-z]+)");
  const r1 = "42".replace(re, "[$<val>]");
  expect(r1).toBe("[42]");
  const r2 = "abc".replace(re, "[$<val>]");
  expect(r2).toBe("[abc]");
});

test("exec with duplicate names and function replacer", () => {
  const re = new RegExp("(?<tag>\\w+)=(\\d+)|(?<tag>\\w+)");
  const result = "foo=42".replace(re, (match, g1, g2, g3, offset, str, groups) => {
    return groups.tag.toUpperCase();
  });
  expect(result).toBe("FOO");
});

test("exec with duplicate names and matchAll", () => {
  const re = new RegExp("(?<v>\\d+)|(?<v>[a-z]+)", "g");
  const matches = [...("42 hello 7".matchAll(re))];
  expect(matches.length).toBe(3);
  expect(matches[0].groups.v).toBe("42");
  expect(matches[1].groups.v).toBe("hello");
  expect(matches[2].groups.v).toBe("7");
});

test("exec with duplicate names in same alternative throws SyntaxError", () => {
  expect(() => {
    new RegExp("(?<x>a)(?<x>b)");
  }).toThrow();
});

test("exec with duplicate names in nested same alternative throws SyntaxError", () => {
  expect(() => {
    new RegExp("(?<x>a(?<x>b))");
  }).toThrow();
});

test("exec with duplicate named backreference in same alternative", () => {
  const re = new RegExp("(?<x>a)\\k<x>|(?<x>b)\\k<x>");
  const result1 = re.exec("aa");
  expect(result1.groups.x).toBe("a");
  const result2 = re.exec("bb");
  expect(result2.groups.x).toBe("b");
});

test("exec with duplicate named groups has null prototype on groups", () => {
  const re = new RegExp("(?<x>a)|(?<x>b)");
  const result = re.exec("a");
  expect(result.groups.toString).toBe(undefined);
});

test("exec with duplicate named groups returns null on non-match", () => {
  const re = new RegExp("(?<x>a)|(?<x>b)");
  const result = re.exec("c");
  expect(result).toBe(null);
});

test("exec with duplicate named groups and global flag collects all matches", () => {
  const re = new RegExp("(?<d>\\d+)|(?<d>[a-z]+)", "g");
  const matches = "123 abc 456".match(re);
  expect(matches.length).toBe(3);
  expect(matches[0]).toBe("123");
  expect(matches[1]).toBe("abc");
  expect(matches[2]).toBe("456");
});

test("exec with duplicate names does not interfere with non-duplicate groups", () => {
  const re = new RegExp("(?<x>a)(?<y>1)|(?<x>b)(?<y>2)");
  const result = re.exec("b2");
  expect(result.groups.x).toBe("b");
  expect(result.groups.y).toBe("2");
});

test("exec with duplicate named backreference outside the disjunction", () => {
  const re = new RegExp("(?:(?<x>a)|(?<x>b))\\k<x>");
  const r1 = re.exec("aa");
  expect(r1[0]).toBe("aa");
  expect(r1.groups.x).toBe("a");
  const r2 = re.exec("bb");
  expect(r2[0]).toBe("bb");
  expect(r2.groups.x).toBe("b");
  expect(re.exec("a")).toBe(null);
  expect(re.exec("b")).toBe(null);
  expect(re.exec("ab")).toBe(null);
});

// --- Greedy quantifier with alternation ---

test("greedy star with alternation picks correct match", () => {
  const m = /(aa|aabaac|ba|b|c)*/.exec("aabaac");
  expect(m[0]).toBe("aaba");
  expect(m[1]).toBe("ba");
});

test("greedy star with character class quantifier backtracks correctly", () => {
  const m = /^([a-z]+)*[a-z]$/.exec("ab");
  expect(m[0]).toBe("ab");
  expect(m[1]).toBe("a");
});

test("backreference backtracking finds correct capture length", () => {
  const m = /^(a+)\1*,\1+$/.exec("aaaaaaaaaa,aaaaaaaaaaaaaaa");
  expect(m[0]).toBe("aaaaaaaaaa,aaaaaaaaaaaaaaa");
  expect(m[1]).toBe("aaaaa");
});

test("replace with backreference uses correct capture", () => {
  expect("aaaaaaaaaa,aaaaaaaaaaaaaaa".replace(/^(a+)\1*,\1+$/, "$1")).toBe("aaaaa");
});

// --- Zero-width backref loop ---

test("backreference to zero-length capture with + does not hang", () => {
  const m = /(a*)b\1+/.exec("baaac");
  expect(m[0]).toBe("b");
  expect(m[1]).toBe("");
});

// --- Backtrack limit ---

test("catastrophic backtracking throws Error instead of hanging", () => {
  expect(() => {
    /^(a+)+$/.exec("a".repeat(30) + "b");
  }).toThrow(Error);
});

// --- Large input (#515 regression) ---

test("exec on large input does not crash", () => {
  const s = "foo" + ".bar".repeat(20000);
  expect(/f.*/.test(s)).toBe(true);
});

// --- Lookahead ---

test("positive lookahead matches without consuming", () => {
  const m = /foo(?=bar)/.exec("foobar");
  expect(m[0]).toBe("foo");
  expect(m.index).toBe(0);
});

test("negative lookahead rejects when pattern present", () => {
  expect(/foo(?!bar)/.test("foobar")).toBe(false);
  expect(/foo(?!bar)/.test("foobaz")).toBe(true);
});

// --- Lookbehind ---

test("positive lookbehind matches fixed-length pattern", () => {
  const m = /(?<=foo)bar/.exec("foobar");
  expect(m[0]).toBe("bar");
  expect(m.index).toBe(3);
});

test("positive lookbehind fails when prefix absent", () => {
  expect(/(?<=foo)bar/.test("bazbar")).toBe(false);
});

test("negative lookbehind rejects when pattern present", () => {
  expect(/(?<!foo)bar/.test("foobar")).toBe(false);
  expect(/(?<!foo)bar/.test("bazbar")).toBe(true);
});

test("lookbehind with alternation", () => {
  const m = "xabcd".match(/.*(?<=(..|...|....))(.*)/);
  expect(m[0]).toBe("xabcd");
  expect(m[1]).toBe("cd");
  expect(m[2]).toBe("");
});

test("lookbehind with quantifier in outer pattern", () => {
  const m = /(?<=\d+)px/.exec("100px");
  expect(m[0]).toBe("px");
  expect(m.index).toBe(3);
});

test("lookbehind does not consume input", () => {
  const m = /(?<=a)b/.exec("ab");
  expect(m[0]).toBe("b");
  expect(m.index).toBe(1);
});
