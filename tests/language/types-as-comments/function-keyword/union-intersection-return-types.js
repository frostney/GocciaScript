/*---
description: Union and intersection return types on function-keyword forms are parsed and ignored at runtime
features: [types-as-comments, compat-function]
---*/

test("function declaration with a union return type", () => {
  function classify(flag: boolean): string | { code: number } {
    return flag ? "ok" : { code: 7 };
  }

  expect(classify(true)).toBe("ok");
  expect(classify(false).code).toBe(7);
});

test("function declaration with an intersection return type", () => {
  function combine(): { a: string } & { b: number } {
    return { a: "s", b: 1 };
  }

  expect(combine().a).toBe("s");
  expect(combine().b).toBe(1);
});

test("function expression with a union of object types", () => {
  const make = function (): { kind: string } | { kind: string; size: number } {
    return { kind: "leaf" };
  };

  expect(make().kind).toBe("leaf");
});

test("generic function declaration with a structured union return type", () => {
  function wrap<T>(value: T): { value: T } | null {
    return { value: value };
  }

  expect(wrap(3).value).toBe(3);
});
