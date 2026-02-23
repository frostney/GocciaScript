/*---
description: Symbol.asyncIterator is a well-known symbol
features: [Symbol.asyncIterator]
---*/

describe("Symbol.asyncIterator", () => {
  test("Symbol.asyncIterator exists", () => {
    expect(Symbol.asyncIterator).toBeTruthy();
  });

  test("Symbol.asyncIterator is a symbol", () => {
    expect(typeof Symbol.asyncIterator).toBe("symbol");
  });

  test("Symbol.asyncIterator description", () => {
    expect(Symbol.asyncIterator.description).toBe("Symbol.asyncIterator");
  });

  test("Symbol.asyncIterator is stable", () => {
    expect(Symbol.asyncIterator).toBe(Symbol.asyncIterator);
  });

  test("can use Symbol.asyncIterator as property key", () => {
    const obj = {
      [Symbol.asyncIterator]() {
        return {
          next() {
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };
    expect(typeof obj[Symbol.asyncIterator]).toBe("function");
  });
});
