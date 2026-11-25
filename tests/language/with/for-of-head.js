/*---
description: A for-of head inside with stores each value into the with object's property of that name, and into the binding only when no with object exposes the name as a binding
features: [compat-var, compat-non-strict-mode]
---*/

var topLevelName = 1;
const topLevelBox = { topLevelName: 0 };
with (topLevelBox) { for (var topLevelName of ["g"]) {} }

describe("for-of head inside with", () => {

test("a var head at the top level stores into the object's property", () => {
  expect([topLevelName, topLevelBox.topLevelName]).toEqual([1, "g"]);
});

test("a var head stores into the object's property", () => {
  const box = { x: 0 };
  const f = () => { var x; with (box) { for (var x of ["s"]) {} } return [x, box.x]; };
  expect(f()).toEqual([undefined, "s"]);
});

test("a var head stores every value into the property", () => {
  const box = { x: 0 };
  const seen = [];
  const f = () => { var x; with (box) { for (var x of ["a", "b"]) { seen.push(box.x); } } return [x, box.x]; };
  expect(f()).toEqual([undefined, "b"]);
  expect(seen).toEqual(["a", "b"]);
});

test("a var head whose name no with object exposes stores into the var", () => {
  const box = {};
  const f = () => { var x; with (box) { for (var x of ["n"]) {} } return [x, "x" in box]; };
  expect(f()).toEqual(["n", false]);
});

test("a var head declared only by the loop stores into the var when no with object exposes the name", () => {
  const box = {};
  const f = () => { with (box) { for (var y of ["u"]) {} } return y; };
  expect(f()).toBe("u");
});

test("an object pattern var head stores into the object's property", () => {
  const box = { x: 0 };
  const f = () => { var x; with (box) { for (var { x } of [{ x: "d" }]) {} } return [x, box.x]; };
  expect(f()).toEqual([undefined, "d"]);
});

test("an array pattern var head stores into the object's property", () => {
  const box = { x: 0 };
  const f = () => { var x; with (box) { for (var [x] of [["e"]]) {} } return [x, box.x]; };
  expect(f()).toEqual([undefined, "e"]);
});

test("a pattern var head stores only the names an object has into it", () => {
  const box = { x: 0 };
  const f = () => { var x, y; with (box) { for (var [x, y] of [["p", "q"]]) {} } return [x, y, box.x, "y" in box]; };
  expect(f()).toEqual([undefined, "q", "p", false]);
});

test("an assignment head stores into the object's property", () => {
  const box = { x: 0 };
  const f = () => { var x; with (box) { for (x of ["f"]) {} } return [x, box.x]; };
  expect(f()).toEqual([undefined, "f"]);
});

test("an assignment head whose name no with object exposes stores into the binding", () => {
  const box = {};
  const f = () => { let x; with (box) { for (x of ["m"]) {} } return [x, "x" in box]; };
  expect(f()).toEqual(["m", false]);
});

test("an object rest assignment head resolves its name before the rest object is built", () => {
  const box = {};
  const source = { get a() { box.rest = "added"; return 1; } };
  const f = () => { var rest; with (box) { for ({ ...rest } of [source]) {} } return [rest, box.rest]; };
  expect(f()).toEqual([{ a: 1 }, "added"]);
});

test("a var head whose store into the object throws closes the iterator", () => {
  const log = [];
  const error = new RangeError("setter");
  const box = { set x(value) { log.push("set"); throw error; } };
  const iterable = {
    [Symbol.iterator]() {
      let done = false;
      return {
        next: () => { const result = { value: 1, done }; done = true; return result; },
        return: () => { log.push("close"); return {}; },
      };
    },
  };
  const f = () => { var x; with (box) { for (var x of iterable) {} } };
  let caught;
  try { f(); } catch (e) { caught = e; }
  expect(caught).toBe(error);
  expect(log).toEqual(["set", "close"]);
});

test("a for-await var head whose store into the object throws closes the iterator", async () => {
  const log = [];
  const error = new RangeError("setter");
  const box = { set x(value) { log.push("set"); throw error; } };
  const iterable = {
    [Symbol.asyncIterator]() {
      let done = false;
      return {
        next: () => { const result = { value: 1, done }; done = true; return Promise.resolve(result); },
        return: () => { log.push("close"); return Promise.resolve({}); },
      };
    },
  };
  const f = async () => { var x; with (box) { for await (var x of iterable) {} } };
  let caught;
  try { await f(); } catch (e) { caught = e; }
  expect(caught).toBe(error);
  expect(log).toEqual(["set", "close"]);
});

test("a property Symbol.unscopables hides is not a binding, so the var receives the value", () => {
  const box = { x: 0, [Symbol.unscopables]: { x: true } };
  const f = () => { var x; with (box) { for (var x of ["u"]) {} } return [x, box.x]; };
  expect(f()).toEqual(["u", 0]);
});

test("a let head declares its own binding", () => {
  const box = { x: 0 };
  const seen = [];
  const f = () => { var x = "outer"; with (box) { for (let x of ["l"]) { seen.push(x); } } return [x, box.x]; };
  expect(f()).toEqual(["outer", 0]);
  expect(seen).toEqual(["l"]);
});

test("the nearest object that has the name receives the value", () => {
  const outer = { x: 0 };
  const inner = { x: 1 };
  const f = () => { var x; with (outer) { with (inner) { for (var x of ["i"]) {} } } return [x, outer.x, inner.x]; };
  expect(f()).toEqual([undefined, 0, "i"]);
});

test("a for-await var head stores into the object's property", async () => {
  const box = { x: 0 };
  const f = async () => { var x; with (box) { for await (var x of ["w"]) {} } return [x, box.x]; };
  expect(await f()).toEqual([undefined, "w"]);
});

test("a for-await assignment head stores into the object's property", async () => {
  const box = { x: 0 };
  const f = async () => { var x; with (box) { for await (x of ["v"]) {} } return [x, box.x]; };
  expect(await f()).toEqual([undefined, "v"]);
});

}); // describe
