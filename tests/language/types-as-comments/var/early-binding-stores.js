/*---
description: An annotated var is checked on stores compiled before its declaration, including a loop's earlier stores
features: [types-as-comments, strict-type-enforcement, compat-var]
---*/

describe("stores compiled before a typed var declaration", () => {
  test("reads before initialization are undefined and loop stores use the annotation", () => {
    const run = () => {
      const seen = [];
      for (const phase of [0, 1]) {
        seen.push(value);
        if (phase === 1) {
          let failure;
          try { value = 5; } catch (error) { failure = error; }
          expect(failure).toBeInstanceOf(TypeError);
          expect(value).toBe("original");
          value = "accepted";
          seen.push(value);
        }
        if (phase === 0) var value: string = "original";
      }
      return seen;
    };

    expect(run()).toEqual([undefined, "original", "accepted"]);
  });

  test("a for-of var head checks the value before a later typed declaration", () => {
    const run = () => {
      for (var value of [5]) {}
      var value: string;
    };
    expect(run).toThrow(TypeError);
  });

  test("a destructuring var default checks the value before a later typed declaration", () => {
    const run = () => {
      var [value = 5] = [];
      var value: string;
    };
    expect(run).toThrow(TypeError);
  });

  test("a typed catch redeclaration applies to the catch binding alone", () => {
    const run = () => {
      try {
        throw "caught";
      } catch (value) {
        expect(() => { value = 5; }).toThrow(TypeError);
        expect(value).toBe("caught");
        var value: string;
      }
      value = 7;
      return value;
    };
    expect(run()).toBe(7);
  });
});
