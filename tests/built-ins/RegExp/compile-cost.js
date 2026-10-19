/*---
description: Compiling a pattern costs time linear in its length for alternation and named groups
features: [RegExp, Goccia]
---*/

// ES2026 §22.2.1 gives a pattern no size limit, so compiling it is one pass
// over its source and costs time linear in its length (#1800). Each `|` used
// to walk every alternative before it, and every named group was compared with
// every other, so 30,000 alternatives took 0.7 s. The cost checks compare two
// sizes on the same machine instead of an absolute time: a pattern 10 times
// longer costs about 10 times as much when the cost is linear, and about 100
// times as much when it is quadratic.

const alternatives = (n) => "a|".repeat(n) + "b";
const namedGroups = (n) =>
  Array.from({ length: n }, (_, i) => `(?<g${i}>a)`).join("");
// One name in every alternative: every pair of groups may share the name.
const duplicateNames = (n) => "(?<a>x)|".repeat(n) + "(?<a>y)";

// The fastest of several rounds, so a collection or a busy machine during one
// round does not decide the result.
const compileTime = (source) => {
  let best = Infinity;
  for (const _ of Array.from({ length: 7 })) {
    const start = performance.now();
    new RegExp(source);
    best = Math.min(best, performance.now() - start);
  }
  return best;
};

const growth = (make, small, large) => {
  const smallSource = make(small);
  const largeSource = make(large);
  return compileTime(largeSource) / compileTime(smallSource);
};

describe.runIf(typeof Goccia !== "undefined")("compile cost", () => {
  test("30,000 alternatives cost at most about 3 times the linear growth over 3,000", () => {
    expect(growth(alternatives, 3000, 30000)).toBeLessThan(30);
  });

  test("8,000 named groups cost at most about 3 times the linear growth over 800", () => {
    expect(growth(namedGroups, 800, 8000)).toBeLessThan(30);
  });

  test("30,000 groups sharing a name across alternatives cost at most about 3 times the linear growth over 3,000", () => {
    expect(growth(duplicateNames, 3000, 30000)).toBeLessThan(30);
  });
});

describe("alternation", () => {
  test("a pattern of 30,000 alternatives matches the last and rejects others", () => {
    const re = new RegExp(alternatives(30000));
    expect(re.test("b")).toBe(true);
    expect(re.test("a")).toBe(true);
    expect(re.test("c")).toBe(false);
    expect(new RegExp(`^(?:${alternatives(30000)})$`).test("ab")).toBe(false);
  });

  test("the first alternative that lets the match succeed wins", () => {
    expect(/a|ab/.exec("ab")[0]).toBe("a");
    expect(/ab|a/.exec("ab")[0]).toBe("ab");
    // A later alternative is tried when the rest of the pattern fails.
    expect(/^(?:a|ab)$/.exec("ab")[0]).toBe("ab");
    expect(/(?:a|ab)(?:c|bcd)/.exec("abcd")[0]).toBe("abcd");
  });

  test("empty alternatives match the empty string", () => {
    expect(/|a/.exec("a")[0]).toBe("");
    expect(/a|/.exec("b")[0]).toBe("");
    expect(/a||b/.exec("b")[0]).toBe("");
    expect(/^(?:a||b)$/.test("b")).toBe(true);
    expect(/^(?:a||b)$/.test("")).toBe(true);
    expect(/^(?:a||b)$/.test("c")).toBe(false);
  });

  test("each alternative sets its own captures and leaves the others undefined", () => {
    const match = /(a)|(b)|(c)/.exec("c");
    expect(match.length).toBe(4);
    expect(match[1]).toBe(undefined);
    expect(match[2]).toBe(undefined);
    expect(match[3]).toBe("c");
  });

  test("alternation inside groups, loops and lookarounds", () => {
    expect(/^(?:a|b|c)+$/.test("abcabc")).toBe(true);
    expect(/^(?:a|b|c)+$/.test("abd")).toBe(false);
    expect(/(?=b|c)./.exec("abc")[0]).toBe("b");
    expect(/(?!a|b)./.exec("abc")[0]).toBe("c");
    expect(/(?<=x|yz)w/.exec("yzw").index).toBe(2);
    expect(/(?<!x|yz)w/.exec("xwyzww").index).toBe(5);
    expect(/(?:a|b)*?c/.exec("abac")[0]).toBe("abac");
  });

  test("a jump over alternatives that contain loops and groups", () => {
    const re = /^(?:(?:ab)+|(?:cd){2}|e(?:f|g)h)i$/;
    expect(re.test("ababi")).toBe(true);
    expect(re.test("cdcdi")).toBe(true);
    expect(re.test("cdi")).toBe(false);
    expect(re.test("egh" + "i")).toBe(true);
    expect(re.test("eh" + "i")).toBe(false);
  });

  test("alternatives with the u and v flags", () => {
    expect(/^(?:\u{1F600}|a|[\u{1F601}-\u{1F64F}])$/u.test("\u{1F602}")).toBe(true);
    expect(/^(?:ab|cd|[\p{L}--[a-z]])$/v.test("Z")).toBe(true);
    expect(/^(?:ab|cd|[\p{L}--[a-z]])$/v.test("z")).toBe(false);
  });
});

describe("nested groups", () => {
  test("captures are numbered by their opening parenthesis", () => {
    const match = /((a)(b(c)))(d)/.exec("abcd");
    expect(match.length).toBe(6);
    expect(match[1]).toBe("abc");
    expect(match[2]).toBe("a");
    expect(match[3]).toBe("bc");
    expect(match[4]).toBe("c");
    expect(match[5]).toBe("d");
  });

  test("a capture inside a repeated alternation is reset on each iteration", () => {
    const match = /(?:(a)|b)*/.exec("ab");
    expect(match[0]).toBe("ab");
    expect(match[1]).toBe(undefined);
  });

  test("1,023 nested groups and 30,000 sibling groups compile and match", () => {
    const deep = new RegExp("(".repeat(1023) + "a" + ")".repeat(1023)).exec("a");
    expect(deep.length).toBe(1024);
    expect(deep[1023]).toBe("a");
    const wide = new RegExp("(a)".repeat(30000)).exec("a".repeat(30000));
    expect(wide.length).toBe(30001);
    expect(wide[30000]).toBe("a");
  });
});

describe("named groups", () => {
  test("every group is on the groups object under its own name", () => {
    const match = /(?<year>\d{4})-(?<month>\d\d)-(?<day>\d\d)/.exec("2026-10-19");
    expect(match.groups.year).toBe("2026");
    expect(match.groups.month).toBe("10");
    expect(match.groups.day).toBe("19");
    expect(Object.keys(match.groups).join()).toBe("year,month,day");
  });

  test("1,000 named groups keep their order and numbers", () => {
    const source = Array.from({ length: 1000 }, (_, i) => `(?<g${i}>${i % 10})`).join("");
    const input = Array.from({ length: 1000 }, (_, i) => `${i % 10}`).join("");
    const match = new RegExp(source).exec(input);
    expect(match.length).toBe(1001);
    expect(match.groups.g0).toBe("0");
    expect(match.groups.g999).toBe("9");
    expect(match[1000]).toBe("9");
    expect(Object.keys(match.groups).length).toBe(1000);
    expect(Object.keys(match.groups)[500]).toBe("g500");
  });

  test("a group name may be written with escapes", () => {
    expect(/(?<ab>x)/.exec("x").groups.ab).toBe("x");
    expect(/(?<\u{61}b>x)\k<ab>/.test("xx")).toBe(true);
  });

  test("a name repeated in different alternatives is allowed", () => {
    const re = /(?<y>\d{4})-\d\d|\d\d-(?<y>\d{4})/;
    expect(re.exec("2026-10").groups.y).toBe("2026");
    expect(re.exec("10-2026").groups.y).toBe("2026");
    expect(/(?:(?<a>x)|(?<a>y))(?:(?<b>1)|(?<b>2))/.exec("y2").groups.b).toBe("2");
    expect(new RegExp(duplicateNames(1000)).exec("y").groups.a).toBe("y");
  });

  test("a name repeated where both groups can take part in one match is a SyntaxError", () => {
    expect(() => new RegExp("(?<a>x)(?<a>y)")).toThrow(SyntaxError);
    expect(() => new RegExp("(?<a>x)(?:(?<a>y)|z)")).toThrow(SyntaxError);
    expect(() => new RegExp("(?:(?<a>x)|y)(?<a>z)")).toThrow(SyntaxError);
    expect(() => new RegExp("(?<a>(?<a>x))")).toThrow(SyntaxError);
    expect(() => new RegExp("(?<a>x)|(?<b>y)(?<b>z)")).toThrow(SyntaxError);
    expect(() => new RegExp("(?<a>x)|(?<a>y)(?<a>w)")).toThrow(SyntaxError);
    expect(() => new RegExp(duplicateNames(1000) + "(?<a>z)")).toThrow(SyntaxError);
  });

  test("a group name must be a valid identifier", () => {
    expect(() => new RegExp("(?<1a>x)")).toThrow(SyntaxError);
    expect(() => new RegExp("(?<>x)")).toThrow(SyntaxError);
  });
});

describe("backreferences", () => {
  test("a numbered backreference matches what its group captured", () => {
    expect(/(a)\1/.test("aa")).toBe(true);
    expect(/(a)\1/.test("ab")).toBe(false);
    expect(/(a|b)\1/.exec("abba")[0]).toBe("bb");
    expect(/((a)|(b))\3/.test("bb")).toBe(true);
  });

  test("a backreference to a group that has not captured matches the empty string", () => {
    expect(/\1(a)/.exec("a")[0]).toBe("a");
    expect(/(?:(a)|b)\1/.exec("b")[0]).toBe("b");
  });

  test("a named backreference matches what its group captured", () => {
    expect(/(?<n>a|b)\k<n>/.exec("abba")[0]).toBe("bb");
    expect(/(?<n>a)\k<n>/.test("ab")).toBe(false);
    expect(/\k<n>(?<n>a)/.test("a")).toBe(true);
  });

  test("a backreference to a name that more than one group has uses the one that took part", () => {
    const re = /(?:(?<a>x)|(?<a>y))\k<a>/;
    expect(re.test("xx")).toBe(true);
    expect(re.test("yy")).toBe(true);
    expect(re.test("xy")).toBe(false);
    expect(re.test("yx")).toBe(false);
    const three = /^(?:(?<a>x)|(?<a>y)|(?<a>z))\k<a>$/;
    expect(three.test("zz")).toBe(true);
    expect(three.test("zx")).toBe(false);
  });

  test("a named backreference to a group that is missing", () => {
    expect(() => new RegExp("(?<a>x)\\k<b>")).toThrow(SyntaxError);
    expect(() => new RegExp("\\k<a>", "u")).toThrow(SyntaxError);
    expect(new RegExp("\\k<a>").test("k<a>")).toBe(true);
  });

  test("backreferences honour the i flag and sit in lookbehinds", () => {
    expect(/(?<n>a)\k<n>/i.test("aA")).toBe(true);
    expect(/(a)\1/i.test("aA")).toBe(true);
    expect(/(?<=(a)\1)b/.test("aab")).toBe(true);
    expect(/(?<=\1(a))b/.test("aab")).toBe(true);
  });

  test("30,000 named backreferences to one group compile and match", () => {
    const re = new RegExp("(?<n>a)" + "\\k<n>".repeat(30000));
    expect(re.test("a".repeat(30001))).toBe(true);
    expect(re.test("a".repeat(30000))).toBe(false);
  });
});
