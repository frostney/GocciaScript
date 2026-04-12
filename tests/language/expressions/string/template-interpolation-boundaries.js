/*---
description: Lexically-aware ${...} boundary detection in template literals
features: [template-literals, interpolation-boundaries]
---*/

describe("template interpolation boundary detection", () => {
  test("string literal containing } inside interpolation", () => {
    const x = `${"}"}`;
    expect(x).toBe("}");
  });

  test("string literal containing { inside interpolation", () => {
    const x = `${"{"}`;
    expect(x).toBe("{");
  });

  test("single-quoted string containing } inside interpolation", () => {
    const x = `${'}'}`;
    expect(x).toBe("}");
  });

  test("string with escaped quote and } inside interpolation", () => {
    const x = `${"\"}\""}`;
    expect(x).toBe("\"}\"");
  });

  test("object literal inside interpolation", () => {
    const x = `${{a: 1}}`;
    expect(x).toBe("[object Object]");
  });

  test("nested object literal inside interpolation", () => {
    const x = `${{a: {b: 2}}}`;
    expect(x).toBe("[object Object]");
  });

  test("block comment containing } inside interpolation", () => {
    const x = `${/*}*/1}`;
    expect(x).toBe("1");
  });

  test("block comment containing { inside interpolation", () => {
    const x = `${/*{*/2}`;
    expect(x).toBe("2");
  });

  test("block comment containing multiple braces", () => {
    const x = `${/*}{}{*/3}`;
    expect(x).toBe("3");
  });

  test("line comment containing } inside interpolation", () => {
    const x = `${
      1 // }
      + 2
    }`;
    expect(x.trim()).toBe("3");
  });

  test("nested template literal inside interpolation", () => {
    const x = `${`nested`}`;
    expect(x).toBe("nested");
  });

  test("nested template with its own interpolation", () => {
    const y = 42;
    const x = `${`inner ${y}`}`;
    expect(x).toBe("inner 42");
  });

  test("nested template containing } in static text", () => {
    const x = `${`a}b`}`;
    expect(x).toBe("a}b");
  });

  test("deeply nested templates", () => {
    const a = 1;
    const b = 2;
    const x = `${`${`${a + b}`}`}`;
    expect(x).toBe("3");
  });

  test("mixed: object literal and string with } in same template", () => {
    const obj = {x: 1};
    const x = `${obj} and ${"}"}`;
    expect(x).toBe("[object Object] and }");
  });

  test("ternary with object literals inside interpolation", () => {
    const cond = true;
    const x = `${cond ? {a: 1} : {b: 2}}`;
    expect(x).toBe("[object Object]");
  });

  test("arrow function with braces inside interpolation", () => {
    const fn = (x) => { return x + 1; };
    const x = `${fn(2)}`;
    expect(x).toBe("3");
  });

  test("array with nested objects inside interpolation", () => {
    const arr = [{a: 1}, {b: 2}];
    const x = `${arr.length}`;
    expect(x).toBe("2");
  });

  test("empty interpolation followed by normal text", () => {
    const x = `${""}end`;
    expect(x).toBe("end");
  });

  test("escaped dollar-brace is not an interpolation boundary", () => {
    const x = `\${not interpolated}`;
    expect(x).toBe("${not interpolated}");
  });

  test("multiple interpolations with complex expressions", () => {
    const a = {x: 1};
    const b = "}";
    const c = /*}*/ 3;
    const x = `${a}|${b}|${c}`;
    expect(x).toBe("[object Object]|}|3");
  });

  test("line comment at end of interpolation expression", () => {
    const x = `${1 // trailing comment
}`;
    expect(x).toBe("1");
  });
});
