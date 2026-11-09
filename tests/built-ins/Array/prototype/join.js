/*---
description: Array.prototype.join joins the elements of an array into a string
features: [Array.prototype.join]
---*/

test("Array.prototype.join joins the elements of an array into a string", () => {
  const arr = [1, 2, 3];
  const joined = arr.join(",");
  expect(joined).toBe("1,2,3");
});

test("Array.prototype.join with custom separator", () => {
  const arr = [1, 2, 3];
  const joined = arr.join("-");
  expect(joined).toBe("1-2-3");
});

test("Array.prototype.join with empty array", () => {
  const arr = [];
  const joined = arr.join(",");
  expect(joined).toBe("");
});

test("join with no separator defaults to comma", () => {
  expect([1, 2, 3].join()).toBe("1,2,3");
});

test("join with undefined separator defaults to comma", () => {
  expect([1, 2, 3].join(undefined)).toBe("1,2,3");
});

test("join with empty string separator", () => {
  expect([1, 2, 3].join("")).toBe("123");
});

test("join converts null and undefined to empty string", () => {
  expect([1, null, undefined, 4].join(",")).toBe("1,,,4");
});

test("join with single element (no separator)", () => {
  expect([42].join(",")).toBe("42");
});

test("join converts elements to strings", () => {
  expect([true, false, 0].join(",")).toBe("true,false,0");
});

test("generic receiver joins array-like elements", () => {
  const arrayLike = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
  expect(Array.prototype.join.call(arrayLike, '-')).toBe('a-b-c');
  expect(Array.prototype.join.call(arrayLike)).toBe('a,b,c');
});

test("primitive this joins as empty string", () => {
  expect(Array.prototype.join.call(true, ',')).toBe('');
});

describe("Array.prototype.join cycles", () => {
  test("generic receivers can contain themselves", () => {
    const object = { length: 2, 0: 1, toString: Array.prototype.toString, join: Array.prototype.join };
    object[1] = object;
    expect(object.join()).toBe("1,");
  });

  test("a failed element conversion does not leave a cycle marker", () => {
    const array = [{ toString() { throw new Error("stop"); } }];
    expect(() => array.join()).toThrow("stop");
    array[0] = 2;
    expect(array.join()).toBe("2");
  });

  test("recursive join reads length and separator before skipping elements", () => {
    const events = [];
    const object = {
      get length() { events.push("length"); return 1; },
      get 0() {
        events.push("element");
        return Array.prototype.join.call(object, {
          toString() { events.push("separator"); return ":"; }
        });
      }
    };
    expect(Array.prototype.join.call(object)).toBe("");
    expect(events).toEqual(["length", "element", "length", "separator"]);
  });
});

describe("Array.prototype.join callback stack protection", () => {
  test("a bound native length getter throws a catchable RangeError", () => {
    const object = {};
    Object.defineProperty(object, "length", {
      get: Array.prototype.join.bind(object)
    });
    expect(() => Array.prototype.join.call(object)).toThrow(RangeError);
    expect([1, 2].join("-")).toBe("1-2");
  });

  test("a bound native separator conversion throws a catchable RangeError", () => {
    const array = [1, 2];
    const separator = {};
    separator.toString = Array.prototype.join.bind(array, separator);
    expect(() => array.join(separator)).toThrow(RangeError);
    expect(array.join("-")).toBe("1-2");
  });
});
