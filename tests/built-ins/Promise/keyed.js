/*---
description: Promise keyed combinators follow Await Dictionary semantics
features: [Promise]
---*/

describe("Promise keyed combinators", () => {
  test("reads the constructor resolve method once", () => {
    const originalResolve = Promise.resolve;
    let getCount = 0;
    let callCount = 0;

    Object.defineProperty(Promise, "resolve", {
      configurable: true,
      get() {
        getCount += 1;
        return (value) => {
          callCount += 1;
          return originalResolve.call(Promise, value);
        };
      },
    });

    Promise.allKeyed({ first: 1, second: 2 });
    expect(getCount).toBe(1);
    expect(callCount).toBe(2);

    Object.defineProperty(Promise, "resolve", {
      configurable: true,
      writable: true,
      value: originalResolve,
    });
  });

  test("allSettledKeyed uses one already-called cell per key", () => {
    let result;
    class Constructor {
      constructor(executor) {
        executor((value) => {
          result = value;
        }, () => {});
      }
    }
    Constructor.resolve = (value) => value;

    Promise.allSettledKeyed.call(Constructor, {
      key: {
        then(onFulfilled, onRejected) {
          onFulfilled("first");
          onRejected("second");
          onFulfilled("third");
        },
      },
    });

    expect(result.key).toEqual({ status: "fulfilled", value: "first" });
  });

  test("allSettledKeyed creates ordinary writable result entries", () => {
    return Promise.allSettledKeyed({ value: Promise.resolve(1) }).then((result) => {
      expect(Object.getPrototypeOf(result)).toBe(null);
      expect(Object.getPrototypeOf(result.value)).toBe(Object.prototype);

      const descriptor = Object.getOwnPropertyDescriptor(result.value, "status");
      expect(descriptor.value).toBe("fulfilled");
      expect(descriptor.writable).toBe(true);
      expect(descriptor.enumerable).toBe(true);
      expect(descriptor.configurable).toBe(true);
    });
  });
});
