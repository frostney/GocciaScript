/*---
description: Template literal types in call and new type arguments are erased; a substitution that is not a type keeps the comparison reading
features: [types-as-comments]
---*/

const identity = (value) => value;

class Box {
  value: number;

  constructor(value = 1) {
    this.value = value;
  }
}

describe("template literal type arguments", () => {
  test("on a call", () => {
    expect(identity<`t${string}`>(1)).toBe(1);
    expect(identity<`${string}-${number}`>(2)).toBe(2);
  });

  test("on a constructor, with and without arguments", () => {
    expect(new Box<`t${string}`>().value).toBe(1);
    expect(new Box<`id-${number}`>(5).value).toBe(5);
  });

  test("among other type arguments", () => {
    expect(identity<() => void, `t${string}`>(3)).toBe(3);
  });

  test("before a tagged template", () => {
    const tag = (strings) => strings.raw.join("|");

    expect(tag<`t${string}`>`a${1}b`).toBe("a|b");
  });

  test("with substitutions of every type a template literal type holds", () => {
    expect(identity<`a${`b${string}`}`>(1)).toBe(1);
    expect(identity<`${Uppercase<Lowercase<string>>}`>(2)).toBe(2);
    expect(identity<`${"x" | "y"}-${number}`>(3)).toBe(3);
    expect(identity<`${keyof { k: 1 }}`>(4)).toBe(4);
    expect(identity<`${{ k: "v" }["k"]}`>(5)).toBe(5);
    expect(identity<`${string extends string ? "a" : "b"}`>(6)).toBe(6);
    expect(identity<`${-1}`>(7)).toBe(7);
  });

  test("with function, array and tuple types as the arguments of a type in a substitution", () => {
    expect(identity<`${ReturnType<() => "a">}`>(1)).toBe(1);
    expect(identity<`${ReturnType<(x: string, ...rest: number[]) => "a">}`>(2)).toBe(2);
    expect(identity<`${[string, number][0]}`>(3)).toBe(3);
    expect(identity<`${[key: "x"][0]}`>(4)).toBe(4);
  });

  test("in a union", () => {
    expect(identity<`a${string}` | "b">(5)).toBe(5);
  });

  test("inside function, tuple, readonly and object types in the list", () => {
    expect(identity<(x: `a${string}`) => void>(1)).toBe(1);
    expect(identity<[`a${string}`, number]>(2)).toBe(2);
    expect(identity<`${keyof { readonly a: 1 }}`>(3)).toBe(3);
    expect(identity<readonly `a${string}`[]>(4)).toBe(4);
  });

  test("as the constraint of a type parameter with a default", () => {
    expect(identity< <T extends `x${string}` = "x">() => T>(5)).toBe(5);
  });
});

describe("comparisons with templates and calls", () => {
  const a = 1;
  const b = 2;
  const c = 0;
  const two = () => 2;

  test("an expression substitution keeps the comparison reading", () => {
    // prettier-ignore
    expect(a < `x${1 + 2}` > (c)).toBe(false);
    // prettier-ignore
    expect(a < `${two()}` > (c)).toBe(true);
    // prettier-ignore
    expect(a < `${a, b}` > (c)).toBe(true);
    // prettier-ignore
    expect(a < `${a ? b : c}` > (c)).toBe(true);
    // prettier-ignore
    expect(a < `${new Box()}` > (c)).toBe(false);
    // prettier-ignore
    expect(a < `${Math.max(1, 2)}` > (c)).toBe(true);
    // prettier-ignore
    expect(a < `${/x/.source}` > (c)).toBe(false);
  });

  test("a tagged template in a substitution keeps the comparison reading", () => {
    const tag = (strings) => strings.raw.join("");

    // prettier-ignore
    expect(a < `${tag`x`}` > (c)).toBe(false);
  });

  test("an operator after a template keeps the comparison reading", () => {
    // TypeScript cannot read `+` after a type, so the span is not type arguments.
    // prettier-ignore
    expect(a < [`${b}` + 1] > (c)).toBe(true);
    // prettier-ignore
    expect(typeof (new Box < `${b}` + 1 > (c))).toBe("boolean");
  });

  test("a template inside brackets or after a tag keeps the comparison reading", () => {
    const tag = () => 7;

    // prettier-ignore
    expect(a < [0 + `${b}`] > (c)).toBe(true);
    // prettier-ignore
    expect(typeof (new Box < tag`${b}` > (c))).toBe("boolean");
  });

  test("a conditional or an index after a template keeps the comparison reading", () => {
    const zero = 0;

    // prettier-ignore
    expect(zero < `${b}` ? 7 : 1 > (c)).toBe(7);
    // prettier-ignore
    expect(typeof (a < `${b}`[0 + 1] > (c))).toBe("boolean");
  });

  test("an expression elsewhere in the list keeps the comparison reading", () => {
    const zero = 0;

    // prettier-ignore
    expect(zero < b ? 7 : `${b}` > (c)).toBe(7);
    // prettier-ignore
    expect(typeof (new Box < 1 + `${b}` > (c))).toBe("boolean");
  });

  test("a tag that is a member named with a keyword keeps the comparison reading", () => {
    const obj = { default: () => 5 };

    // prettier-ignore
    expect(typeof (a < obj.default`${b}` > (c))).toBe("boolean");
  });

  test("a tag that is a member named like a type keyword keeps the comparison reading", () => {
    const obj = { keyof: () => 5 };

    // prettier-ignore
    expect(typeof (a < obj.keyof`${b}` > (c))).toBe("boolean");
    // prettier-ignore
    expect(typeof (new Box < obj.keyof`${b}` > (c))).toBe("boolean");
  });

  test("a line break before an index keeps the comparison reading", () => {
    // prettier-ignore
    expect(typeof (a < `${b
[0]}` > (c))).toBe("boolean");
  });

  test("a call between the angle brackets keeps the comparison reading", () => {
    // prettier-ignore
    expect(a < two() > (c)).toBe(true);
  });

  test("type arguments inside a substitution of a failed probe", () => {
    const z = "0";

    // prettier-ignore
    expect(a < `${identity<`t${string}`>(1)}${z}${z}${z}${z}${z}${z}${z}${z}${z}${z}${z}${z}` > b).toBe(false);
  });

  test("type arguments after a long comparison chain ending in templates", () => {
    const chainThenCall = a < b < a < b < a < b < a < b < a < b < a < b < a < b < a < `${identity<`t${string}`>(2)}` < identity<`u${number}`>(3);

    expect(chainThenCall).toBe(true);
  });

  test("type arguments after a chain that fails at an expression template", () => {
    const results = [
      a < `${a + b}` < b < a < b < a < b < a < b < a < b < a < b < a < `${identity<`v${string}`>(4)}${identity<`w${string}`>(5)}`,
      identity<`x${string}`>(6),
    ];

    expect(results).toEqual([true, 6]);
  });
});
