/*---
description: Under strict types, an enforceable catch parameter annotation checks the caught value and every later store
features: [types-as-comments, strict-type-enforcement]
---*/

describe("catch parameter enforcement", () => {
  test("a caught value of the annotated type is bound", () => {
    let caught;
    try {
      throw "message";
    } catch (e: string) {
      caught = e;
    }
    expect(caught).toBe("message");
  });

  test("a caught value of another type throws a TypeError from the clause", () => {
    let entered = false;
    const run = () => {
      try {
        throw "message";
      } catch (e: number) {
        entered = true;
      }
    };
    expect(run).toThrow(TypeError);
    expect(entered).toBe(false);
  });

  test("the TypeError reports the annotated type", () => {
    const run = () => {
      try {
        throw 1;
      } catch (e: string) {
        return e;
      }
    };
    expect(run).toThrow("Type 'number' is not assignable to type 'string'");
  });

  test("an object type rejects a thrown primitive", () => {
    const run = () => {
      try {
        throw 1;
      } catch (e: object) {
        return e;
      }
    };
    expect(run).toThrow(TypeError);
  });

  test("a later store into the parameter is checked", () => {
    const run = (next) => {
      try {
        throw 1;
      } catch (e: number) {
        e = next;
        return e;
      }
    };
    expect(run(2)).toBe(2);
    expect(() => run("two")).toThrow(TypeError);
  });

  test("a closure store into the parameter is checked", () => {
    let set;
    try {
      throw 1;
    } catch (e: number) {
      set = (value) => { e = value; };
    }
    set(2);
    expect(() => set("two")).toThrow(TypeError);
  });

  test("the finally block still runs when the caught value is rejected", () => {
    const steps = [];
    const run = () => {
      try {
        throw "message";
      } catch (e: number) {
        steps.push("catch");
      } finally {
        steps.push("finally");
      }
    };
    expect(run).toThrow(TypeError);
    expect(steps).toEqual(["finally"]);
  });
});
