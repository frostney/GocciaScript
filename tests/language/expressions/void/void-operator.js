/*---
description: Void operator evaluates operand and returns undefined
features: [void-operator]
---*/

test("void 0 returns undefined", () => {
  expect(void 0).toBe(undefined);
});

test("void with various literals returns undefined", () => {
  expect(void 1).toBe(undefined);
  expect(void "hello").toBe(undefined);
  expect(void true).toBe(undefined);
  expect(void false).toBe(undefined);
  expect(void null).toBe(undefined);
  expect(void undefined).toBe(undefined);
});

test("void with expressions returns undefined", () => {
  expect(void (1 + 2)).toBe(undefined);
  expect(void (3 * 4)).toBe(undefined);
});

test("void evaluates operand for side effects", () => {
  let x = 0;
  void (x = 5);
  expect(x).toBe(5);
});

test("void with function call evaluates the call", () => {
  let called = false;
  const sideEffect = () => { called = true; };
  void sideEffect();
  expect(called).toBe(true);
});

test("typeof void 0 is undefined", () => {
  expect(typeof (void 0)).toBe("undefined");
});

test("void has correct precedence", () => {
  expect(void 0 === undefined).toBe(true);
});

test("void with object returns undefined", () => {
  expect(void {}).toBe(undefined);
  expect(void []).toBe(undefined);
});

test("void with variable returns undefined", () => {
  const x = 42;
  expect(void x).toBe(undefined);
});
