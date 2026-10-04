/*---
description: Apply function programatically
features: [function-apply]
---*/

test("apply function programatically", () => {
  const add = (a, b) => a + b;
  const result = add.apply(null, [1, 2]);
  expect(result).toBe(3);
});

test("apply function on object", () => {
  const obj = {
    value: 1,
    add: (a, b) => a + b,
  };
  const result = obj.add.apply(obj, [2, 3]);
  expect(result).toBe(5);
});

test("apply passes every argument for any argument count", () => {
  const collect = (...args) => args.length + ":" + args.join("");
  const three = (a, b, c) => [a, b, c].join("|");
  const many = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];

  expect(collect.apply(null)).toBe("0:");
  expect(collect.apply(null, [])).toBe("0:");
  expect(collect.apply(null, [1])).toBe("1:1");
  expect(collect.apply(null, [1, 2])).toBe("2:12");
  expect(collect.apply(null, [1, 2, 3])).toBe("3:123");
  expect(collect.apply(null, [1, 2, 3, 4])).toBe("4:1234");
  expect(collect.apply(null, many.slice(0, 8))).toBe("8:12345678");
  expect(collect.apply(null, many.slice(0, 9))).toBe("9:123456789");
  expect(collect.apply(null, many)).toBe("12:123456789101112");
  expect(three.apply(null, [1])).toBe("1||");
  expect(three.apply(null, many)).toBe("1|2|3");
});

test("apply reads a sparse or array-like argument list", () => {
  const collect = (...args) => args.map((value) => String(value)).join(",");

  expect(collect.apply(null, [1, , 3])).toBe("1,undefined,3");
  expect(collect.apply(null, { length: 2, 0: "x", 1: "y" })).toBe("x,y");
});

test("apply binds this on a method for any argument count", () => {
  const holder = {
    tag: "T",
    join(...parts) {
      return this.tag + parts.join("");
    },
  };
  const other = { tag: "U" };

  expect(holder.join.apply(other, [])).toBe("U");
  expect(holder.join.apply(other, [1, 2, 3])).toBe("U123");
  expect(holder.join.apply(other, [1, 2, 3, 4, 5, 6, 7, 8, 9])).toBe("U123456789");
});
