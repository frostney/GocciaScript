/*---
description: Generator yield resumes computed public class field names
features: [compat-function, computed-property-names, class-fields-public, class-static-fields-public, generators]
---*/

test("yield resumes computed public instance and static field names", () => {
  function* makeClass() {
    let C = class {
      [yield "instance-key"] = 9;
      static [yield "static-key"] = 10;
    };
    let c = new C();
    return [c, C];
  }

  const instanceKey = Symbol("instance");
  const staticKey = Symbol("static");
  const iter = makeClass();

  expect(iter.next()).toEqual({ value: "instance-key", done: false });
  expect(iter.next(instanceKey)).toEqual({ value: "static-key", done: false });

  const result = iter.next(staticKey);
  expect(result.done).toBe(true);
  expect(result.value[0][instanceKey]).toBe(9);
  expect(result.value[1][staticKey]).toBe(10);
});

test("omitted next values install fields under undefined keys", () => {
  let observed = [];

  function* makeClass() {
    let C = class {
      [yield 9] = 9;
      static [yield 9] = 10;
    };
    let c = new C();
    observed = [c[undefined], C[undefined], c[9], C[9]];
  }

  const iter = makeClass();

  expect(iter.next()).toEqual({ value: 9, done: false });
  expect(iter.next()).toEqual({ value: 9, done: false });
  expect(iter.next()).toEqual({ value: undefined, done: true });
  expect(observed).toEqual([9, 10, undefined, undefined]);
});

test("computed field initializers from yield remain callable", () => {
  function* makeClass() {
    let C = class {
      [yield 9] = () => 9;
      static [yield 9] = () => 10;
    };
    let c = new C();
    return [
      c[yield 9](),
      C[yield 9](),
      c[String(yield 9)](),
      C[String(yield 9)](),
    ];
  }

  const iter = makeClass();

  expect(iter.next()).toEqual({ value: 9, done: false });
  expect(iter.next(9)).toEqual({ value: 9, done: false });
  expect(iter.next(9)).toEqual({ value: 9, done: false });
  expect(iter.next(9)).toEqual({ value: 9, done: false });
  expect(iter.next(9)).toEqual({ value: 9, done: false });
  expect(iter.next(9)).toEqual({ value: 9, done: false });
  expect(iter.next(9)).toEqual({ value: [9, 10, 9, 10], done: true });
});
