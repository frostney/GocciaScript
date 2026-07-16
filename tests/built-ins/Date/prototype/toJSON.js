/*---
description: Date.prototype.toJSON
features: [Date]
---*/

test("returns the same representation as toISOString", () => {
  const date = new Date(1718451045123);
  expect(date.toJSON()).toBe(date.toISOString());
});

test("remains generic for object receivers", () => {
  const receiver = {
    valueOf() {
      return "not-a-number";
    },
    toISOString() {
      return "generic";
    },
  };
  expect(Date.prototype.toJSON.call(receiver)).toBe("generic");
});

test("returns null only for non-finite numeric primitives", () => {
  const receiver = {
    valueOf() {
      return NaN;
    },
    toISOString() {
      return "unreachable";
    },
  };
  expect(Date.prototype.toJSON.call(receiver)).toBeNull();
});
