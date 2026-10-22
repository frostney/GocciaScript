/*---
description: Assigning to an array index the array does not own runs [[Set]] on the prototype chain (ES2026 10.1.9.2 OrdinarySetWithOwnDescriptor)
features: [Array, Proxy, class]
---*/

// ES2026 §10.1.9.2 OrdinarySetWithOwnDescriptor step 1: an index the array
// does not own, a hole or one past the end, is [[Set]] on the prototype with
// the array as the receiver. An inherited setter is called, an inherited
// read-only property or getter-only accessor makes the store fail, which
// §6.2.5.6 PutValue step 3.e turns into a TypeError, and a Proxy prototype
// gets its set trap. A walk that finds nothing creates the element.

describe("arr[i] = v on an index the array does not own", () => {
  const withProto = (arr, descriptors) => {
    Object.setPrototypeOf(arr, Object.create(Array.prototype, descriptors));
    return arr;
  };

  test("a hole calls an inherited setter with the array as receiver", () => {
    const calls = [];
    const arr = withProto([0, , 2], {
      1: { set(value) { calls.push([value, this === arr]); }, configurable: true },
    });
    arr[1] = "x";
    expect(calls).toEqual([["x", true]]);
    expect(Object.hasOwn(arr, 1)).toBe(false);
    expect(arr.length).toBe(3);
  });

  test("an index past the end calls an inherited setter and leaves the length", () => {
    const calls = [];
    const arr = withProto([0], {
      5: { set(value) { calls.push(value); }, configurable: true },
    });
    arr[5] = "x";
    expect(calls).toEqual(["x"]);
    expect(Object.hasOwn(arr, 5)).toBe(false);
    expect(arr.length).toBe(1);
  });

  test("an inherited setter on Array.prototype itself is called", () => {
    const calls = [];
    Object.defineProperty(Array.prototype, "2", {
      set(value) { calls.push(value); },
      configurable: true,
    });
    try {
      const arr = [0];
      arr[2] = "x";
      expect(calls).toEqual(["x"]);
      expect(Object.hasOwn(arr, 2)).toBe(false);
      expect(arr.length).toBe(1);
    } finally {
      delete Array.prototype[2];
    }
  });

  test("an inherited read-only index rejects the assignment", () => {
    const arr = withProto([0, , 2], {
      1: { value: "ro", writable: false, configurable: true },
    });
    expect(() => { arr[1] = "x"; }).toThrow(TypeError);
    expect(Object.hasOwn(arr, 1)).toBe(false);
    expect(arr[1]).toBe("ro");
  });

  test("an inherited getter without a setter rejects the assignment", () => {
    const arr = withProto([0, , 2], {
      1: { get() { return "g"; }, configurable: true },
    });
    expect(() => { arr[1] = "x"; }).toThrow(TypeError);
    expect(Object.hasOwn(arr, 1)).toBe(false);
    expect(arr[1]).toBe("g");
  });

  test("a Proxy prototype's set trap receives the key, value and the array", () => {
    const calls = [];
    const arr = [0, , 2];
    Object.setPrototypeOf(arr, new Proxy(Array.prototype, {
      set(target, key, value, receiver) {
        calls.push([key, value, receiver === arr]);
        return true;
      },
    }));
    arr[1] = "x";
    arr[7] = "y";
    expect(calls).toEqual([["1", "x", true], ["7", "y", true]]);
    expect(Object.hasOwn(arr, 1)).toBe(false);
    expect(arr.length).toBe(3);
  });

  test("a Proxy prototype's set trap returning false rejects the assignment", () => {
    const arr = [0, , 2];
    Object.setPrototypeOf(arr, new Proxy(Array.prototype, { set() { return false; } }));
    expect(() => { arr[1] = "x"; }).toThrow(TypeError);
    expect(Object.hasOwn(arr, 1)).toBe(false);
  });

  test("a Proxy prototype without a set trap creates the element on the array", () => {
    const arr = [0, , 2];
    Object.setPrototypeOf(arr, new Proxy(Array.prototype, {}));
    arr[1] = "x";
    arr[4] = "y";
    expect(Object.hasOwn(arr, 1)).toBe(true);
    expect(arr[1]).toBe("x");
    expect(arr[4]).toBe("y");
    expect(arr.length).toBe(5);
  });

  test("a writable index on a modified Array.prototype is shadowed by a new own element", () => {
    Array.prototype[3] = "inherited";
    try {
      const arr = [0, 1];
      arr[3] = "own";
      const sparse = [0, , 2];
      sparse[1] = "own";
      expect(Object.hasOwn(arr, 3)).toBe(true);
      expect(arr[3]).toBe("own");
      expect(arr.length).toBe(4);
      expect(Object.hasOwn(sparse, 1)).toBe(true);
      expect(sparse[1]).toBe("own");
      expect(Array.prototype[3]).toBe("inherited");
    } finally {
      delete Array.prototype[3];
    }
  });

  test("a string key and a far index take the same path", () => {
    const calls = [];
    const far = 2 ** 32 - 2;
    const arr = withProto([0, , 2], {
      1: { set(value) { calls.push(value); }, configurable: true },
      [far]: { set(value) { calls.push(value); }, configurable: true },
    });
    const key = "1";
    arr[key] = "computed";
    arr["1"] = "literal";
    arr[far] = "far";
    expect(calls).toEqual(["computed", "literal", "far"]);
    expect(Object.hasOwn(arr, 1)).toBe(false);
    expect(Object.hasOwn(arr, far)).toBe(false);
    expect(arr.length).toBe(3);
  });

  test("a setter on an Array subclass's prototype is called", () => {
    class Vec extends Array {
      set 1(value) { this.seen = value; }
    }
    const vec = new Vec();
    vec[0] = "a";
    vec[1] = "b";
    vec[2] = "c";
    expect(vec.seen).toBe("b");
    expect(Object.hasOwn(vec, 1)).toBe(false);
    expect(vec[0]).toBe("a");
    expect(vec[2]).toBe("c");
    expect(vec.length).toBe(3);
  });

  test("a non-extensible array still calls an inherited setter", () => {
    const calls = [];
    const arr = withProto([0, , 2], {
      1: { set(value) { calls.push(value); }, configurable: true },
    });
    Object.preventExtensions(arr);
    arr[1] = "x";
    expect(calls).toEqual(["x"]);
  });

  test("a frozen array still calls an inherited setter for a hole", () => {
    const calls = [];
    const arr = withProto([0, , 2], {
      1: { set(value) { calls.push(value); }, configurable: true },
    });
    Object.freeze(arr);
    arr[1] = "x";
    expect(calls).toEqual(["x"]);
  });

  test("a read-only length still lets an inherited setter take an index past the end", () => {
    const calls = [];
    const arr = withProto([0], {
      5: { set(value) { calls.push(value); }, configurable: true },
    });
    Object.defineProperty(arr, "length", { writable: false });
    arr[5] = "x";
    expect(calls).toEqual(["x"]);
    expect(arr.length).toBe(1);
  });

  test("an element of a frozen array used as the prototype is read-only", () => {
    const arr = [0, , 2];
    Object.setPrototypeOf(arr, Object.freeze(Object.setPrototypeOf(["p0", "p1"], Array.prototype)));
    expect(() => { arr[1] = "x"; }).toThrow(TypeError);
    expect(Object.hasOwn(arr, 1)).toBe(false);
    expect(arr[1]).toBe("p1");
  });

  test("a prototype cycle through a Proxy ends in a RangeError", () => {
    const arr = [0, , 2];
    Object.setPrototypeOf(arr, new Proxy(arr, {}));
    expect(() => { arr[5] = 1; }).toThrow(RangeError);
    expect(() => { arr["5"] = 1; }).toThrow(RangeError);
  });

  test("a non-extensible array rejects a new element when nothing is inherited at the index", () => {
    const arr = withProto([0, , 2], {
      9: { value: 1, writable: true, configurable: true },
    });
    Object.preventExtensions(arr);
    expect(() => { arr[1] = "x"; }).toThrow(TypeError);
    expect(Object.hasOwn(arr, 1)).toBe(false);
  });
});

describe("arr[i] = v on an element the array owns", () => {
  test("writes the element without consulting an inherited setter", () => {
    const calls = [];
    const arr = [0, 1, 2];
    arr.named = true;
    Object.setPrototypeOf(arr, Object.create(Array.prototype, {
      1: { set(value) { calls.push(value); }, configurable: true },
    }));
    arr[1] = "x";
    expect(calls).toEqual([]);
    expect(arr[1]).toBe("x");
  });

  test("a frozen array rejects the write even when the prototype has a writable index", () => {
    Array.prototype[1] = "p";
    try {
      const arr = Object.freeze([0, 1]);
      expect(() => { arr[1] = 5; }).toThrow(TypeError);
      expect(arr[1]).toBe(1);
    } finally {
      delete Array.prototype[1];
    }
  });
});

describe("arr[i] = v with an unmodified prototype chain", () => {
  test("fills a hole, appends and extends the length", () => {
    const arr = [0, , 2];
    arr[1] = 1;
    arr[3] = 3;
    arr[10] = 10;
    expect(Object.hasOwn(arr, 1)).toBe(true);
    expect(arr.length).toBe(11);
    expect(arr[10]).toBe(10);
  });

  test("a frozen array rejects filling a hole", () => {
    const arr = Object.freeze([0, , 2]);
    expect(() => { arr[1] = 1; }).toThrow(TypeError);
  });

  test("a non-extensible array rejects an append", () => {
    const arr = Object.preventExtensions([0]);
    expect(() => { arr[1] = 1; }).toThrow(TypeError);
    expect(arr.length).toBe(1);
  });
});
