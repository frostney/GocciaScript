/*---
description: Symbol-keyed assignment on primitive receivers runs prototype setters with the primitive as receiver
features: [Symbol, Object.defineProperty]
---*/

test("symbol-keyed setter on String.prototype runs for a string receiver", () => {
  const S = Symbol("stringSetter");
  let captured = null;
  let receiverType = null;
  Object.defineProperty(String.prototype, S, {
    set(v) {
      captured = v;
      receiverType = typeof this;
    },
    configurable: true,
  });
  "abc"[S] = 42;
  expect(captured).toBe(42);
  expect(receiverType).toBe("string");
});

test("symbol-keyed setter on Number.prototype runs for a number receiver", () => {
  const S = Symbol("numberSetter");
  let captured = null;
  Object.defineProperty(Number.prototype, S, {
    set(v) {
      captured = v;
    },
    configurable: true,
  });
  (5)[S] = "stored";
  expect(captured).toBe("stored");
});

test("symbol-keyed data write to a primitive receiver throws TypeError", () => {
  const S = Symbol("noSetter");
  expect(() => {
    "abc"[S] = 1;
  }).toThrow(TypeError);
});
