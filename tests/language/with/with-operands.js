/*---
description: Identifier operands inside a with statement resolve against the object first, and bindings declared inside the body stay local
features: [with, let, compat-non-strict-mode]
---*/

const pass = (value) => value;

describe("operands inside a with statement", () => {
  test("an object property shadows a parameter and a let binding", () => {
    const run = (a, scope) => {
      let b = a + 1;
      const outside = a + b;
      let inside;
      with (scope) {
        inside = a + b;
      }
      return [outside, inside, a + b];
    };

    expect(run(pass(1), pass({}))).toEqual([3, 3, 3]);
    expect(run(pass(1), pass({ a: 10 }))).toEqual([3, 12, 3]);
    expect(run(pass(1), pass({ a: 10, b: 20 }))).toEqual([3, 30, 3]);
  });

  test("a property added while the body runs is picked up by later operands", () => {
    const run = (a, scope) => {
      const seen = [];
      with (scope) {
        seen.push(a + 1);
        scope.a = 100;
        seen.push(a + 1);
        a = a + 1;
        seen.push(a + 1);
      }
      seen.push(a + 1, scope.a);
      return seen;
    };

    expect(run(pass(1), pass({}))).toEqual([2, 101, 102, 2, 101]);
  });

  test("a binding declared inside the body is not looked up on the object", () => {
    const run = (scope) => {
      const seen = [];
      with (scope) {
        let local = pass(5);
        seen.push(local + 1);
        local = local * 2;
        seen.push(local + 1, shared + 1);
      }
      return seen;
    };

    expect(run(pass({ local: 100, shared: 1 }))).toEqual([6, 11, 2]);
  });

  test("an assignment inside the body writes the object when it has the name", () => {
    const run = (a, scope) => {
      with (scope) {
        a = a + 5;
        a += 1;
      }
      return [a + 1, scope.a];
    };

    expect(run(pass(1), pass({}))).toEqual([8, undefined]);
    expect(run(pass(1), pass({ a: 10 }))).toEqual([2, 16]);
  });
});
