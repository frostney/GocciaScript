/*---
description: Under strict types, ++ and -- on a binding typed other than number throw a TypeError before the binding changes, as assigning the new value would
features: [types-as-comments, strict-type-enforcement]
---*/

import { label, count, results } from "./helpers/update-module-binding.js";

describe("update expressions on strictly typed bindings", () => {

test("postfix ++ on a string parameter throws", () => {
  const f = (x: string) => { x++; return x; };
  expect(() => f("1")).toThrow(TypeError);
});

test("prefix ++ on a string parameter throws", () => {
  const f = (x: string) => { ++x; return x; };
  expect(() => f("1")).toThrow(TypeError);
});

test("postfix -- on a boolean parameter throws", () => {
  const f = (x: boolean) => { x--; return x; };
  expect(() => f(true)).toThrow(TypeError);
});

test("prefix -- on a boolean parameter throws", () => {
  const f = (x: boolean) => { --x; return x; };
  expect(() => f(true)).toThrow(TypeError);
});

test("++ on an object parameter throws", () => {
  const f = (x: object) => { x++; return x; };
  expect(() => f({})).toThrow(TypeError);
});

test("the operand is converted to a number before the check", () => {
  const hints = [];
  const operand = { [Symbol.toPrimitive](hint) { hints.push(hint); return 1; } };
  const f = (x: object) => { x++; };
  expect(() => f(operand)).toThrow(TypeError);
  expect(hints).toEqual(["number"]);
});

test("an error from the conversion propagates instead of the check's", () => {
  const operand = { valueOf() { throw new RangeError("conversion"); } };
  const f = (x: object) => { x++; };
  expect(() => f(operand)).toThrow(RangeError);
});

test("++ on a rest parameter whose conversion gives a BigInt throws", () => {
  const f = (...xs: number) => {
    xs[Symbol.toPrimitive] = () => 1n;
    let threw = false;
    try { xs++; } catch (e) { threw = e instanceof TypeError; }
    return [threw, Array.isArray(xs)];
  };
  expect(f(1)).toEqual([true, true]);
});

test("++ on a typed let in the same function throws", () => {
  let s: string = "1";
  let threw = false;
  try { s++; } catch (e) { threw = e instanceof TypeError; }
  expect(threw).toBe(true);
  expect(s).toBe("1");
});

test("the binding keeps its value when the update throws", () => {
  const f = (x: string) => {
    try { x++; } catch (e) { return [e instanceof TypeError, x, typeof x]; }
    return [false, x, typeof x];
  };
  expect(f("1")).toEqual([true, "1", "string"]);
});

test("a postfix result is not stored when the update throws", () => {
  const f = (x: string) => {
    let r = "unchanged";
    try { r = x++; } catch (e) {}
    return [r, x];
  };
  expect(f("1")).toEqual(["unchanged", "1"]);
});

test("a prefix result is not stored when the update throws", () => {
  const f = (x: string) => {
    let r = "unchanged";
    try { r = --x; } catch (e) {}
    return [r, x];
  };
  expect(f("1")).toEqual(["unchanged", "1"]);
});

test("a binding a closure captures is checked when its own function updates it", () => {
  let s: string = "1";
  const read = () => s;
  let threw = false;
  try { s++; } catch (e) { threw = e instanceof TypeError; }
  expect(threw).toBe(true);
  expect(read()).toBe("1");
});

test("an update inside a closure is checked", () => {
  let s: string = "1";
  const bump = () => { s++; };
  expect(() => bump()).toThrow(TypeError);
  expect(s).toBe("1");
});

test("a binding typed by its literal initializer is checked", () => {
  const f = () => {
    let s = "1";
    try { s++; } catch (e) { return [e instanceof TypeError, s]; }
    return [false, s];
  };
  expect(f()).toEqual([true, "1"]);
});

test("a number binding updates as before", () => {
  const f = (x: number) => {
    const a = x++;
    const b = ++x;
    const c = x--;
    const d = --x;
    return [a, b, c, d, x];
  };
  expect(f(1)).toEqual([1, 3, 3, 1, 1]);
});

test("postfix ++ on a module's top-level binding throws", () => {
  expect(results.postfixThrew).toBe(true);
});

test("prefix -- on a module's top-level binding throws", () => {
  expect(results.prefixThrew).toBe(true);
});

test("a module's top-level binding and its export keep their value", () => {
  expect(label).toBe("1");
});

test("a module's top-level number binding updates as before", () => {
  expect([results.countBefore, count]).toEqual([1, 2]);
});

test("an untyped binding updates as before", () => {
  const f = (x) => { const old = x++; return [old, x]; };
  expect(f("1")).toEqual([1, 2]);
});

}); // describe
