/*---
description: Call function programatically
features: [function-call]
---*/

test("call function programatically", () => {
  const add = (a, b) => a + b;
  const result = add.call(null, 1, 2);
  expect(result).toBe(3);
});

test("call function on object", () => {
  const obj = {
    value: 1,
    add: (a, b) => a + b,
  };
  const result = obj.add.call(obj, 2, 3);
  expect(result).toBe(5);
});

test("call passes every argument for any argument count", () => {
  const collect = (...args) => args.length + ":" + args.join("");
  const three = (a, b, c) => [a, b, c].join("|");

  expect(collect.call()).toBe("0:");
  expect(collect.call(null)).toBe("0:");
  expect(collect.call(null, 1)).toBe("1:1");
  expect(collect.call(null, 1, 2)).toBe("2:12");
  expect(collect.call(null, 1, 2, 3)).toBe("3:123");
  expect(collect.call(null, 1, 2, 3, 4)).toBe("4:1234");
  expect(collect.call(null, 1, 2, 3, 4, 5, 6, 7, 8)).toBe("8:12345678");
  expect(collect.call(null, 1, 2, 3, 4, 5, 6, 7, 8, 9)).toBe("9:123456789");
  expect(three.call(null, 1)).toBe("1||");
  expect(three.call(null, 1, 2, 3, 4, 5, 6)).toBe("1|2|3");
});

test("call binds this on a method for any argument count", () => {
  const holder = {
    tag: "T",
    join(...parts) {
      return this.tag + parts.join("");
    },
  };
  const other = { tag: "U" };

  expect(holder.join.call(other)).toBe("U");
  expect(holder.join.call(other, 1, 2, 3)).toBe("U123");
  expect(holder.join.call(other, 1, 2, 3, 4, 5, 6, 7, 8, 9)).toBe("U123456789");
});
