/*---
description: Return type annotations on functions are parsed and ignored at runtime
features: [types-as-comments]
---*/

test("arrow function return type", () => {
  const double = (x: number): number => x * 2;
  expect(double(5)).toBe(10);
});

test("arrow function with block body and return type", () => {
  const square = (x: number): number => {
    return x * x;
  };
  expect(square(4)).toBe(16);
});

test("void return type", () => {
  let called = false;
  const doSomething = (): void => {
    called = true;
  };
  doSomething();
  expect(called).toBe(true);
});

test("complex return type", () => {
  const makePoint = (): { x: number, y: number } => {
    return { x: 1, y: 2 };
  };
  const p = makePoint();
  expect(p.x).toBe(1);
  expect(p.y).toBe(2);
});

test("union return type", () => {
  const maybe = (flag: boolean): string | null => {
    if (flag) {
      return "yes";
    }
    return null;
  };
  expect(maybe(true)).toBe("yes");
  expect(maybe(false)).toBe(null);
});
