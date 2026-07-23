/*---
description: Iterator.prototype.includes proposal behavior
features: [Iterator.prototype.includes]
---*/

describe("Iterator.prototype.includes()", () => {
  test("has the proposal-defined function shape", () => {
    const descriptor = Object.getOwnPropertyDescriptor(
      Iterator.prototype,
      "includes",
    );

    expect(typeof descriptor.value).toBe("function");
    expect(descriptor.value.name).toBe("includes");
    expect(descriptor.value.length).toBe(1);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(true);
    expect(descriptor.writable).toBe(true);
    expect(() => new Iterator.prototype.includes()).toThrow(TypeError);
  });

  test("returns a boolean and consumes through the matching value", () => {
    const iterator = [1, 2, 3].values();

    expect(iterator.includes(2)).toBe(true);
    expect(iterator.next().value).toBe(3);
    expect([1, 2, 3].values().includes(4)).toBe(false);
  });

  test("does not internally close a native iterator without return", () => {
    const matched = new Map([[1, "one"], [2, "two"]]).values();
    expect(matched.includes("one")).toBe(true);
    expect(matched.next().value).toBe("two");

    const validationError = new Set([1, 2]).values();
    expect(() => validationError.includes(1, "0")).toThrow(TypeError);
    expect(validationError.next().value).toBe(1);
  });

  test("searches for undefined when searchElement is omitted", () => {
    expect([1, undefined, 3].values().includes()).toBe(true);
  });

  test("accepts direct plain iterator objects", () => {
    const iterator = {
      count: 0,
      next() {
        this.count = this.count + 1;
        return this.count <= 3
          ? { value: this.count, done: false }
          : { value: undefined, done: true };
      },
    };

    expect(Iterator.prototype.includes.call(iterator, 2)).toBe(true);
    expect(iterator.count).toBe(2);
  });

  test("uses SameValueZero for numbers", () => {
    expect([NaN].values().includes(NaN)).toBe(true);
    expect([-0].values().includes(0)).toBe(true);
    expect([0].values().includes(-0)).toBe(true);
  });

  test("uses identity for objects and symbols", () => {
    const object = {};
    const sameDescription = Symbol("value");
    const symbol = Symbol("value");

    expect([object].values().includes(object)).toBe(true);
    expect([{}].values().includes(object)).toBe(false);
    expect([symbol].values().includes(symbol)).toBe(true);
    expect([sameDescription].values().includes(symbol)).toBe(false);
  });

  test("skips the requested number of leading elements", () => {
    expect([1, 2, 1].values().includes(1, 1)).toBe(true);
    expect([1, 2, 3].values().includes(1, 1)).toBe(false);
    expect([1].values().includes(1, -0)).toBe(true);
    expect([1].values().includes(1, undefined)).toBe(true);
    expect([1].values().includes(1, Number.MAX_SAFE_INTEGER)).toBe(false);
  });

  test("allows Infinity and consumes the iterator to exhaustion", () => {
    let returnAccessed = false;
    const iterator = {
      count: 0,
      next() {
        this.count = this.count + 1;
        return this.count <= 2
          ? { value: 1, done: false }
          : { value: undefined, done: true };
      },
    };
    Object.defineProperty(iterator, "return", {
      get() {
        returnAccessed = true;
        return () => ({});
      },
    });

    expect(Iterator.prototype.includes.call(iterator, 1, Infinity)).toBe(false);
    expect(iterator.count).toBe(3);
    expect(returnAccessed).toBe(false);
  });

  test("rejects non-integral and non-Number skippedElements without coercion", () => {
    const invalidValues = [
      NaN,
      -0.5,
      0.5,
      "1",
      1n,
      true,
      null,
      {},
      Symbol("1"),
    ];

    invalidValues.forEach((invalidValue) => {
      expect(() => [1].values().includes(1, invalidValue)).toThrow(TypeError);
    });

    let coerced = 0;
    let closed = 0;
    const skippedElements = {
      valueOf() {
        coerced = coerced + 1;
        return 0;
      },
    };
    const iterator = {
      next() {
        return { value: 1, done: false };
      },
      return() {
        closed = closed + 1;
        return {};
      },
    };

    expect(() => Iterator.prototype.includes.call(
      iterator,
      1,
      skippedElements,
    )).toThrow(TypeError);
    expect(coerced).toBe(0);
    expect(closed).toBe(1);
  });

  test("rejects negative and too-large skippedElements", () => {
    expect(() => [1].values().includes(1, -1)).toThrow(RangeError);
    expect(() => [1].values().includes(1, -Infinity)).toThrow(RangeError);
    expect(() => [1].values().includes(
      1,
      Number.MAX_SAFE_INTEGER + 1,
    )).toThrow(RangeError);
  });

  test("closes before reading next when skippedElements is invalid", () => {
    let nextAccessed = false;
    let closed = 0;
    const iterator = {
      return() {
        closed = closed + 1;
        return {};
      },
    };
    Object.defineProperty(iterator, "next", {
      get() {
        nextAccessed = true;
        return () => ({ done: true });
      },
    });

    expect(() => Iterator.prototype.includes.call(iterator, 1, 0.5))
      .toThrow(TypeError);
    expect(nextAccessed).toBe(false);
    expect(closed).toBe(1);
  });

  test("preserves validation errors when return access or calls fail", () => {
    const getterError = {};
    Object.defineProperty(getterError, "return", {
      get() {
        throw new RangeError("close getter");
      },
    });

    expect(() => Iterator.prototype.includes.call(getterError, 1, "0"))
      .toThrow(TypeError);

    const methodError = {
      return() {
        throw new TypeError("close method");
      },
    };
    expect(() => Iterator.prototype.includes.call(methodError, 1, -1))
      .toThrow(RangeError);
  });

  test("rejects primitive receivers before skippedElements validation", () => {
    expect(() => Iterator.prototype.includes.call(null, 1, 0)).toThrow(TypeError);
    expect(() => Iterator.prototype.includes.call(1, 1, 0)).toThrow(TypeError);
  });

  test("gets next once after validating skippedElements", () => {
    let nextGets = 0;
    let calls = 0;
    const iterator = {};
    Object.defineProperty(iterator, "next", {
      get() {
        nextGets = nextGets + 1;
        return () => {
          calls = calls + 1;
          return calls === 1
            ? { value: 1, done: false }
            : { value: undefined, done: true };
        };
      },
    });

    expect(Iterator.prototype.includes.call(iterator, 2)).toBe(false);
    expect(nextGets).toBe(1);
    expect(calls).toBe(2);
  });

  test("does not close when getting next throws", () => {
    let closed = 0;
    const iterator = {
      return() {
        closed = closed + 1;
        return {};
      },
    };
    Object.defineProperty(iterator, "next", {
      get() {
        throw new Error("next getter");
      },
    });

    expect(() => Iterator.prototype.includes.call(iterator, 1)).toThrow(Error);
    expect(closed).toBe(0);
  });

  test("does not close when next is invalid or throws", () => {
    let closed = 0;
    const invalidNext = {
      next: 0,
      return() {
        closed = closed + 1;
        return {};
      },
    };
    expect(() => Iterator.prototype.includes.call(invalidNext, 1))
      .toThrow(TypeError);
    expect(closed).toBe(0);

    const throwingNext = {
      next() {
        throw new Error("next call");
      },
      return() {
        closed = closed + 1;
        return {};
      },
    };
    expect(() => Iterator.prototype.includes.call(throwingNext, 1))
      .toThrow(Error);
    expect(closed).toBe(0);
  });

  test("does not close when next returns a primitive", () => {
    let closed = 0;
    const iterator = {
      next() {
        return 1;
      },
      return() {
        closed = closed + 1;
        return {};
      },
    };

    expect(() => Iterator.prototype.includes.call(iterator, 1)).toThrow(TypeError);
    expect(closed).toBe(0);
  });

  test("does not close when done or value access throws", () => {
    let closed = 0;
    const doneError = {
      next() {
        const result = {};
        Object.defineProperty(result, "done", {
          get() {
            throw new Error("done");
          },
        });
        return result;
      },
      return() {
        closed = closed + 1;
        return {};
      },
    };
    expect(() => Iterator.prototype.includes.call(doneError, 1)).toThrow(Error);
    expect(closed).toBe(0);

    const valueError = {
      next() {
        const result = { done: false };
        Object.defineProperty(result, "value", {
          get() {
            throw new Error("value");
          },
        });
        return result;
      },
      return() {
        closed = closed + 1;
        return {};
      },
    };
    expect(() => Iterator.prototype.includes.call(valueError, 1)).toThrow(Error);
    expect(closed).toBe(0);
  });

  test("closes with the direct iterator as receiver after a match", () => {
    let returnThis;
    const iterator = {
      next() {
        return { value: 1, done: false };
      },
      return() {
        returnThis = this;
        return {};
      },
    };

    expect(Iterator.prototype.includes.call(iterator, 1)).toBe(true);
    expect(returnThis).toBe(iterator);
  });

  test("propagates return failures after a match", () => {
    const getterError = {
      next() {
        return { value: 1, done: false };
      },
    };
    Object.defineProperty(getterError, "return", {
      get() {
        throw new Error("return getter");
      },
    });
    expect(() => Iterator.prototype.includes.call(getterError, 1)).toThrow(Error);

    const methodError = {
      next() {
        return { value: 1, done: false };
      },
      return() {
        throw new Error("return call");
      },
    };
    expect(() => Iterator.prototype.includes.call(methodError, 1)).toThrow(Error);

    const invalidResult = {
      next() {
        return { value: 1, done: false };
      },
      return() {
        return 0;
      },
    };
    expect(() => Iterator.prototype.includes.call(invalidResult, 1))
      .toThrow(TypeError);
  });

  test("does not access return after normal exhaustion", () => {
    let returnAccessed = false;
    const iterator = {
      next() {
        return { value: undefined, done: true };
      },
    };
    Object.defineProperty(iterator, "return", {
      get() {
        returnAccessed = true;
        return () => ({});
      },
    });

    expect(Iterator.prototype.includes.call(iterator, 1)).toBe(false);
    expect(returnAccessed).toBe(false);
  });
});
