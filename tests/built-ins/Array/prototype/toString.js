describe("Array.prototype.toString", () => {
  test("exists on Array.prototype", () => {
    expect(typeof Array.prototype.toString).toBe("function");
  });

  test("returns comma-separated string for simple array", () => {
    expect([1, 2, 3].toString()).toBe("1,2,3");
  });

  test("returns empty string for empty array", () => {
    expect([].toString()).toBe("");
  });

  test("single element array", () => {
    expect([42].toString()).toBe("42");
  });

  test("mixed types", () => {
    expect([1, "a", true, null, undefined].toString()).toBe("1,a,true,,");
  });

  test("nested arrays", () => {
    expect([1, [2, 3], 4].toString()).toBe("1,2,3,4");
  });

  test("string elements", () => {
    expect(["hello", "world"].toString()).toBe("hello,world");
  });

  test("boolean elements", () => {
    expect([true, false].toString()).toBe("true,false");
  });

  test("toString is used in string concatenation", () => {
    expect([1, 2] + [3, 4]).toBe("1,23,4");
    expect("" + [1, 2, 3]).toBe("1,2,3");
  });

  test("toString is used in template literals", () => {
    const arr = [1, 2, 3];
    expect(`${arr}`).toBe("1,2,3");
  });

  test("array with plain objects", () => {
    expect([{}, 1].toString()).toBe("[object Object],1");
  });

  test("array with custom toString objects", () => {
    const obj = { toString() { return "custom"; } };
    expect([obj, 1, 2].toString()).toBe("custom,1,2");
  });

  test("array with custom valueOf (string hint prefers toString)", () => {
    const obj = { valueOf() { return 10; }, toString() { return "str"; } };
    expect([obj].toString()).toBe("str");
  });

  test("array with toString returning non-primitive falls back to valueOf", () => {
    const obj = { valueOf() { return "val"; }, toString() { return {}; } };
    expect([obj].toString()).toBe("val");
  });
});

describe("Array.prototype.join edge cases", () => {
  test("join with empty string separator", () => {
    expect([1, 2, 3].join("")).toBe("123");
  });

  test("join with multi-character separator", () => {
    expect([1, 2, 3].join(" - ")).toBe("1 - 2 - 3");
  });

  test("join with undefined separator defaults to comma", () => {
    expect([1, 2, 3].join(undefined)).toBe("1,2,3");
  });

  test("join with null elements", () => {
    expect([null, null].join(",")).toBe(",");
  });

  test("join with undefined elements", () => {
    expect([undefined, undefined].join(",")).toBe(",");
  });

  test("join with mixed null/undefined/values", () => {
    expect([1, null, 2, undefined, 3].join(",")).toBe("1,,2,,3");
  });

  test("join on single-element array", () => {
    expect([42].join(",")).toBe("42");
  });

  test("join on empty array", () => {
    expect([].join(",")).toBe("");
  });

  test("join with boolean separator", () => {
    expect([1, 2].join(true)).toBe("1true2");
  });

  test("join with number separator", () => {
    expect([1, 2].join(0)).toBe("102");
  });

  test("join with Symbol.toStringTag objects", () => {
    const obj = { [Symbol.toStringTag]: "Custom", toString() { return "tagged"; } };
    expect([obj].join(",")).toBe("tagged");
  });
});

describe("Array.prototype.toString with special values", () => {
  test("array of NaN values", () => {
    expect([NaN, NaN].toString()).toBe("NaN,NaN");
  });

  test("array of Infinity values", () => {
    expect([Infinity, -Infinity].toString()).toBe("Infinity,-Infinity");
  });

  test("array with boolean values", () => {
    expect([true, false, true].toString()).toBe("true,false,true");
  });

  test("deeply nested arrays", () => {
    expect([1, [2, [3, [4]]]].toString()).toBe("1,2,3,4");
  });
});


describe("Array string conversion", () => {
  test("template null element is empty", () => {
    expect(`${[null]}`).toBe("");
  });

  test("nested null element is empty in concatenation", () => {
    expect([1, [null]] + "").toBe("1,");
  });

  test("template undefined element is empty", () => {
    expect(`${[undefined, 2]}`).toBe(",2");
  });

  test("String null element control", () => {
    expect(String([null, 1])).toBe(",1");
  });

  test("concatenation null element control", () => {
    expect([null] + "").toBe("");
  });

  test("non-callable own join uses the object tag on every path", () => {
    const array = [1, 2];
    array.join = 0;
    expect(`${array}`).toBe("[object Array]");
    expect(array + "").toBe("[object Array]");
    expect(String(array)).toBe("[object Array]");
    expect([array].toString()).toBe("[object Array]");
    expect(new Error(array).message).toBe("[object Array]");
    const object = { [array]: 7 };
    expect(object["[object Array]"]).toBe(7);
  });

  test("callable own join receives the array and no arguments", () => {
    const array = [null];
    array.join = { method(...args) {
      expect(this).toBe(array);
      expect(args.length).toBe(0);
      return "custom";
    } }.method;
    expect(`${array}`).toBe("custom");
    expect([array].toString()).toBe("custom");
    expect(new Error(array).message).toBe("custom");
  });

  test("nullish nested arrays agree for keys and Error messages", () => {
    const array = [null, [undefined, 2]];
    expect(new Error(array).message).toBe(",,2");
    const object = { [array]: 7 };
    expect(object[",,2"]).toBe(7);
  });

  test("null-prototype arrays have no implicit string conversion", () => {
    const array = [1, 2];
    Object.setPrototypeOf(array, null);
    expect(() => `${array}`).toThrow(TypeError);
    expect(() => new Error(array)).toThrow(TypeError);
    expect(() => ({ [array]: 1 })).toThrow(TypeError);
    array.toString = () => "explicit";
    expect(`${array}`).toBe("explicit");
  });

  test("self-cycle contributes an empty string on every path", () => {
    const array = [1];
    array.push(array);
    expect(array + "").toBe("1,");
    expect(`${array}`).toBe("1,");
    expect(String(array)).toBe("1,");
    expect(new Error(array).message).toBe("1,");
    const object = { [array]: 7 };
    expect(object["1,"]).toBe(7);
  });

  test("mutual and deeper cycles preserve surrounding elements", () => {
    const a = [1];
    const b = [2, a, 3];
    a.push(b, 4);
    expect(`${a}`).toBe("1,2,,3,4");
    const c = [5, a, 6];
    b[1] = c;
    expect(`${a}`).toBe("1,2,5,,6,3,4");
  });

  test("shared children are converted again after a join completes", () => {
    const child = [null, 2];
    expect(`${[child, child]}`).toBe(",2,,2");
  });

  test("moderately nested arrays preserve their leaf", () => {
    let array = [null, 1, undefined];
    for (const unused of Array.from({ length: 128 })) {
      array = [array];
    }
    expect(`${array}`).toBe(",1,");
  });

  test("long nested arrays complete or throw a catchable RangeError", () => {
    let array = [1];
    for (const unused of Array.from({ length: 10000 })) {
      array = [array];
    }
    try {
      expect(`${array}`).toBe("1");
    } catch (error) {
      expect(error instanceof RangeError).toBe(true);
    }
    expect(`${[null, 2]}`).toBe(",2");
  });
});
