/*---
description: Temporal Dead Zone enforcement for let declarations
features: [let, temporal-dead-zone]
---*/

describe("Temporal Dead Zone", () => {
  test("accessing let before declaration throws", () => {
    expect(() => {
      const val = x;
      let x = 10;
    }).toThrow();
  });

  test("accessing const before declaration throws", () => {
    expect(() => {
      const val = y;
      const y = 20;
    }).toThrow();
  });

  test("typeof on undeclared variable returns undefined", () => {
    expect(typeof nonExistentVar).toBe("undefined");
  });

  test("let in block scope does not leak", () => {
    let outer = "outer";
    {
      let inner = "inner";
      expect(inner).toBe("inner");
    }
    expect(() => {
      const val = inner;
    }).toThrow();
  });

  test("let in nested blocks shadows outer", () => {
    let x = 1;
    {
      let x = 2;
      expect(x).toBe(2);
    }
    expect(x).toBe(1);
  });

  test("const reassignment throws TypeError", () => {
    const a = 42;
    expect(() => {
      a = 99;
    }).toThrow();
  });

  test("const object properties can still be mutated", () => {
    const obj = { x: 1 };
    obj.x = 2;
    expect(obj.x).toBe(2);
  });

  test("const array elements can still be mutated", () => {
    const arr = [1, 2, 3];
    arr[0] = 99;
    expect(arr[0]).toBe(99);
  });

  test("multiple let declarations in same scope", () => {
    let a = 1;
    let b = 2;
    let c = 3;
    expect(a + b + c).toBe(6);
  });

  test("let can be reassigned", () => {
    let x = 1;
    x = 2;
    expect(x).toBe(2);
  });

  test("deeply nested scope shadowing", () => {
    let x = "a";
    {
      let x = "b";
      {
        let x = "c";
        expect(x).toBe("c");
      }
      expect(x).toBe("b");
    }
    expect(x).toBe("a");
  });
});
