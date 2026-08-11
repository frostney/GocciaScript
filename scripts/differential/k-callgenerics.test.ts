// Differential suite K — call-site type arguments, and the comparison chains
// they must not swallow.
//
// `<` after a callee is ambiguous with the relational operator. Reading a
// comparison chain as a type argument list is the dangerous direction: it
// raises no error, it just evaluates something else. Every guard below is a
// valid JavaScript comparison whose value would change under a wrong reading,
// so this suite fails loudly rather than silently if the disambiguation drifts.
//
// This file is `.ts`: call-site type arguments are TypeScript-only syntax, and
// the same source in a `.js` file must stay relational. That half is covered by
// tests/language/types-as-comments/generic-arrow-functions.js, which bun cannot
// gate because it would need the same source under two extensions.
const identity = (value) => value;
const holder = { fn: (value) => value };
const tag = (parts) => "tagged:" + parts[0];

const a = 1;
const b = 2;
const c = false;

describe("call-site type arguments", () => {
  test("type arguments on a method call are erased, not compared", () => {
    const result = holder.fn<() => Promise<string>>(() => Promise.resolve("x"));
    expect(typeof result).toBe("function");
  });

  test("type arguments on a builtin method call", () => {
    expect(["a"].map<string>((x) => x)[0]).toBe("a");
  });

  test("nested type arguments closing on '>>'", () => {
    expect(["a"].map<Map<string, number>>((x) => x)[0]).toBe("a");
  });

  test("type arguments on a plain identifier callee", () => {
    expect(identity<string>("v")).toBe("v");
  });

  test("type arguments before a tagged template", () => {
    expect(tag<string>`x`).toBe("tagged:x");
  });

  test("union and intersection type arguments", () => {
    expect(identity<string | number>(1)).toBe(1);
    expect(identity<{ a: 1 } & { b: 2 }>("v")).toBe("v");
  });
});

describe("comparison chains the probe must decline", () => {
  test("a < b > c stays a comparison chain", () => {
    expect(a < b > c).toBe(true);
  });

  test("less-than before a parenthesized expression", () => {
    expect(a < (b + 1)).toBe(true);
  });

  test("a logical operator disqualifies the type-argument reading", () => {
    expect(a < b && c > (a)).toBe(false);
  });

  test("an arithmetic operator disqualifies the type-argument reading", () => {
    expect(a < b - 1).toBe(false);
  });

  test("an unclosed '<' stays relational", () => {
    expect(a < b === true).toBe(true);
  });
});
