/*---
description: Reading the length of a String
features: [String]
---*/

const lengthOf = (value) => value.length;

test("a string primitive's length counts UTF-16 code units", () => {
  expect(lengthOf("")).toBe(0);
  expect(lengthOf("abc")).toBe(3);
  expect(lengthOf("\u{1F600}")).toBe(2);
  expect(lengthOf("é")).toBe(1);
});

test("length read in a loop condition follows the string it reads", () => {
  const holder = { text: "abcd" };
  let count = 0;
  for (const _ of [0, 1, 2, 3, 4, 5]) {
    if (count < holder.text.length) {
      count += 1;
    }
  }
  expect(count).toBe(4);
  holder.text = "ab";
  expect(lengthOf(holder.text)).toBe(2);
});

test("a String object and a value on the prototype chain keep their own reads", () => {
  expect(lengthOf(new String("xyz"))).toBe(3);
  expect(() => {
    Object.defineProperty(String.prototype, "length", { value: 99 });
  }).toThrow(TypeError);
  expect(lengthOf("xyz")).toBe(3);
});
