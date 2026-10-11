/*---
description: structuredClone clones Date, RegExp and primitive wrapper objects as the same kind
features: [structuredClone, Date, RegExp]
---*/

describe("Date cloning", () => {
  test("keeps the time value", () => {
    const original = new Date(5);
    const clone = structuredClone(original);
    expect(Object.prototype.toString.call(clone)).toBe("[object Date]");
    expect(clone instanceof Date).toBe(true);
    expect(Object.getPrototypeOf(clone)).toBe(Date.prototype);
    expect(clone.getTime()).toBe(5);
    expect(clone).not.toBe(original);
  });

  test("keeps an invalid Date invalid", () => {
    expect(Number.isNaN(structuredClone(new Date(NaN)).getTime())).toBe(true);
  });

  test("drops own properties and a subclass prototype", () => {
    class LaterDate extends Date {}
    const original = new LaterDate(1);
    original.extra = 1;
    const clone = structuredClone(original);
    expect(clone.extra).toBeUndefined();
    expect(Object.getPrototypeOf(clone)).toBe(Date.prototype);
    expect(clone.getTime()).toBe(1);
  });

  test("one Date cloned twice is one clone", () => {
    const date = new Date(1);
    const [first, second] = structuredClone([date, date]);
    expect(first).toBe(second);
  });

  // StructuredDeserialize creates the Date directly; no script-visible
  // function takes part.
  test("runs no script-visible function", () => {
    const original = new Date(5);
    const trunc = Math.trunc;
    const set = WeakMap.prototype.set;
    Math.trunc = () => 999;
    WeakMap.prototype.set = () => {
      throw new Error("called");
    };
    try {
      const clone = structuredClone(original);
      expect(clone.getTime()).toBe(5);
    } finally {
      Math.trunc = trunc;
      WeakMap.prototype.set = set;
    }
  });

  test("an object that only inherits from Date.prototype is not a Date", () => {
    const clone = structuredClone(Object.create(Date.prototype));
    expect(Object.prototype.toString.call(clone)).toBe("[object Object]");
  });
});

describe("RegExp cloning", () => {
  test("keeps the source and flags", () => {
    const clone = structuredClone(/a+/gi);
    expect(Object.prototype.toString.call(clone)).toBe("[object RegExp]");
    expect(Object.getPrototypeOf(clone)).toBe(RegExp.prototype);
    expect(clone.source).toBe("a+");
    expect(clone.flags).toBe("gi");
    expect(clone.test("xAA")).toBe(true);
  });

  test("starts at lastIndex 0 and drops own properties", () => {
    const original = /a/g;
    original.lastIndex = 3;
    original.extra = 1;
    const clone = structuredClone(original);
    expect(clone.lastIndex).toBe(0);
    expect(clone.extra).toBeUndefined();
  });

  test("one RegExp cloned twice is one clone", () => {
    const regexp = /a/;
    const [first, second] = structuredClone([regexp, regexp]);
    expect(first).toBe(second);
  });

  test("keeps the sticky, unicode and dotAll flags", () => {
    expect(structuredClone(/\u{1F600}/suy).flags).toBe("suy");
  });
});

describe("primitive wrapper cloning", () => {
  test("a Number object stays a Number object", () => {
    const clone = structuredClone(Object(-0));
    expect(typeof clone).toBe("object");
    expect(Object.prototype.toString.call(clone)).toBe("[object Number]");
    expect(Object.is(clone.valueOf(), -0)).toBe(true);
  });

  test("a String object stays a String object", () => {
    const clone = structuredClone(Object("ab"));
    expect(Object.prototype.toString.call(clone)).toBe("[object String]");
    expect(clone.valueOf()).toBe("ab");
    expect(clone.length).toBe(2);
    expect(clone[1]).toBe("b");
  });

  test("a Boolean object stays a Boolean object", () => {
    const clone = structuredClone(Object(false));
    expect(Object.prototype.toString.call(clone)).toBe("[object Boolean]");
    expect(clone.valueOf()).toBe(false);
  });

  test("a BigInt object stays a BigInt object", () => {
    const clone = structuredClone(Object(5n));
    expect(typeof clone).toBe("object");
    expect(Object.prototype.toString.call(clone)).toBe("[object BigInt]");
    expect(clone.valueOf()).toBe(5n);
  });

  test("drops own properties", () => {
    const original = Object(1);
    original.extra = 1;
    expect(structuredClone(original).extra).toBeUndefined();
  });

  test("one wrapper cloned twice is one clone", () => {
    const wrapper = Object("s");
    const [first, second] = structuredClone([wrapper, wrapper]);
    expect(first).toBe(second);
  });
});
