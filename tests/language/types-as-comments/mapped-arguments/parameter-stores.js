/*---
description: Stores through a mapped arguments index enforce the type of the parameter it aliases
features: [arguments-object, compat-non-strict-mode, function-declarations, strict-type-enforcement]
---*/

// Each typed function's parameter type codes are left behind in freed memory,
// and the next function to need codes may get that block back. An untyped
// parameter before a typed one must stay unchecked whatever the block held.
// These run at the top level, in source order, as the compiler's allocations
// do not repeat this way inside a test callback.
function typedFirst(p0: boolean, p1: boolean, p2: boolean, p3: boolean, p4: boolean, p5: boolean, p6: boolean) { return arguments; }
function untypedFirst(q0, q1, q2, q3, q4, q5, q6: number) { arguments[0] = "x"; return typeof q0; }
function typedSecond(p0: boolean, p1: boolean, p2: boolean, p3: boolean, p4: boolean, p5: boolean, p6: boolean) { return arguments; }
function untypedSecond(r0, r1, r2, r3, r4, r5, r6: number) { arguments[0] = "x"; return typeof r0; }
const untypedResults = [];
for (const [typed, untyped] of [[typedFirst, untypedFirst], [typedSecond, untypedSecond]]) {
  typed(true, true, true, true, true, true, true);
  try {
    untypedResults.push(untyped(1, 2, 3, 4, 5, 6, 7));
  } catch (error) {
    untypedResults.push(`${error.name}: ${error.message}`);
  }
}


describe("strictly typed mapped parameters", () => {
  test("indexed assignments check their value before updating the parameter", () => {
    function run(value: number) {
      const alias = arguments;
      expect(() => { alias["0"] = "wrong"; }).toThrow(TypeError);
      expect([value, alias[0]]).toEqual([2, 2]);
      alias[0] = 4;
      return [value + 1, alias[0]];
    }

    expect(run(2)).toEqual([5, 4]);
  });

  test("updates and destructuring stores use the same check", () => {
    function run(value: string) {
      const alias = arguments;
      expect(() => { alias[0]++; }).toThrow(TypeError);
      expect(() => { [alias[0] = 5] = []; }).toThrow(TypeError);
      expect(() => { alias[0] -= 1; }).toThrow(TypeError);
      return [value, alias[0]];
    }

    expect(run("2")).toEqual(["2", "2"]);
  });

  test("reflective sets honor the receiver and the mapped type", () => {
    function run(value: string) {
      const alias = arguments;
      const receiver = {};
      expect(Reflect.set(alias, "0", 5, receiver)).toBe(true);
      expect(receiver[0]).toBe(5);
      expect(() => Reflect.set(alias, "0", 6)).toThrow(TypeError);
      expect(() => { new Proxy(alias, {})[0] = 7; }).toThrow(TypeError);
      expect(Reflect.set(alias, "0", "accepted")).toBe(true);
      return [value, alias[0]];
    }

    expect(run("original")).toEqual(["accepted", "accepted"]);
  });

  test("a rejected descriptor leaves the property and its mapping intact", () => {
    function run(value: string) {
      const alias = arguments;
      Object.defineProperty(alias, "0", { configurable: false });
      expect(Reflect.defineProperty(alias, "0", { value: 5, configurable: true })).toBe(false);
      expect(() => Object.defineProperty(alias, "0", { value: 5, writable: false })).toThrow(TypeError);
      expect(Object.getOwnPropertyDescriptor(alias, "0").writable).toBe(true);
      expect([value, alias[0]]).toEqual(["original", "original"]);
      expect(Reflect.defineProperty(alias, "0", { value: "accepted" })).toBe(true);
      Object.defineProperty(alias, "0", { writable: false });
      value = "unmapped";
      return [value, alias[0]];
    }

    expect(run("original")).toEqual(["unmapped", "accepted"]);
  });

  test("public class fields use the mapped index's definition semantics", () => {
    function run(value: string) {
      return [arguments, () => value];
    }
    class ReturnObject {
      constructor(target) { return target; }
    }
    class Incompatible extends ReturnObject {
      0 = 5;
    }
    class Compatible extends ReturnObject {
      ["0"] = "accepted";
    }

    const [alias, read] = run("original");
    expect(() => new Incompatible(alias)).toThrow(TypeError);
    expect([read(), alias[0]]).toEqual(["original", "original"]);
    new Compatible(alias);
    expect([read(), alias[0]]).toEqual(["accepted", "accepted"]);
  });

  test("deleting the mapped property lets a replacement hold any value", () => {
    function run(value: string) {
      delete arguments[0];
      arguments[0] = 5;
      return [value, arguments[0]];
    }

    expect(run("original")).toEqual(["original", 5]);
  });

  test("a strict function's arguments object stays unmapped", () => {
    function run(value: string) {
      "use strict";
      arguments[0] = 5;
      return [value, arguments[0]];
    }

    expect(run("original")).toEqual(["original", 5]);
  });

  test("a mapping that outlives its call still checks writes", () => {
    function run(value: string) {
      return [arguments, () => value];
    }

    const [alias, read] = run("original");
    expect(() => { alias[0] = 5; }).toThrow(TypeError);
    expect(read()).toBe("original");
    alias[0] = "accepted";
    expect(read()).toBe("accepted");
    expect(alias).toHaveProperty("0", "accepted");
  });

  test("a suspended generator's mapped parameter still checks writes", () => {
    function* run(value: string) {
      yield arguments;
      return value;
    }

    const generator = run("original");
    const alias = generator.next().value;
    expect(() => { alias[0] = 5; }).toThrow(TypeError);
    alias[0] = "accepted";
    expect(generator.next()).toEqual({ value: "accepted", done: true });
  });

  test("a suspended async function's mapped parameter still checks writes", async () => {
    let alias;
    let resume;
    const pending = new Promise((resolve) => { resume = resolve; });
    async function run(value: string) {
      alias = arguments;
      await pending;
      return value;
    }

    const result = run("original");
    try {
      expect(() => { alias[0] = 5; }).toThrow(TypeError);
      alias[0] = "accepted";
    } finally {
      resume();
    }
    expect(await result).toBe("accepted");
  });

  test("an untyped parameter before a typed one stays unchecked", () => {
    expect(untypedResults).toEqual(["string", "string"]);
  });
});
