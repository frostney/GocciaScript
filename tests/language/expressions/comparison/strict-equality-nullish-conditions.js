/*---
description: A condition comparing a value strictly with null or undefined matches only that value
features: [strict-equality]
---*/

const kinds = [null, undefined, 0, -0, "", false, NaN, {}, [], 0n];

describe("=== and !== null or undefined as branch conditions", () => {
  test("each operand order matches only the value of that kind", () => {
    for (const x of kinds) {
      expect(x === null ? 1 : 0).toBe(Object.is(x, null) ? 1 : 0);
      expect(null === x ? 1 : 0).toBe(Object.is(x, null) ? 1 : 0);
      expect(x !== undefined ? 1 : 0).toBe(Object.is(x, undefined) ? 0 : 1);
      expect(undefined !== x ? 1 : 0).toBe(Object.is(x, undefined) ? 0 : 1);
      let taken = false;
      if (x === undefined) { taken = true; }
      expect(taken).toBe(Object.is(x, undefined));
    }
  });

  test("a missing argument, an array hole and a missing property compare as undefined", () => {
    const f = (a) => (a === undefined ? "missing" : "given");
    expect(f()).toBe("missing");
    expect(f(null)).toBe("given");
    const holes = [1, , 3];
    expect(holes[1] === undefined ? 1 : 0).toBe(1);
    expect({}.missing !== undefined ? 1 : 0).toBe(0);
  });

  test("a const binding of null or undefined compares like the literal", () => {
    const NONE = null;
    const UNSET = undefined;
    for (const x of kinds) {
      expect(x === NONE ? 1 : 0).toBe(Object.is(x, null) ? 1 : 0);
      expect(UNSET !== x ? 1 : 0).toBe(Object.is(x, undefined) ? 0 : 1);
    }
  });

  test("a local named undefined is compared by its value", () => {
    const blockLocal = (x) => {
      let undefined = 5;
      return x === undefined ? "match" : "no";
    };
    expect(blockLocal(5)).toBe("match");
    expect(blockLocal(void 0)).toBe("no");
    const parameter = (x, undefined) => (x === undefined ? "match" : "no");
    expect(parameter(1, 1)).toBe("match");
    expect(parameter(void 0, 1)).toBe("no");
    let caught = "";
    try {
      throw 7;
    } catch (undefined) {
      caught = 7 === undefined ? "match" : "no";
    }
    expect(caught).toBe("match");
  });

  test("a const read before its declaration still throws", () => {
    const early = () => {
      const read = () => (LATER === null ? 1 : 0);
      expect(() => read()).toThrow(ReferenceError);
      const LATER = null;
      return read();
    };
    expect(early()).toBe(1);
  });
});
