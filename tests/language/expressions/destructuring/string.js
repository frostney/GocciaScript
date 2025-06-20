/*---
description: Destructuring strings
features: [string-destructuring]
---*/

test("destructuring strings", () => {
  let word = "Hello";
  let [h, e, l1, l2, o] = word;
  expect(h).toBe("H");
  expect(e).toBe("e");
  expect(l1).toBe("l");
  expect(l2).toBe("l");
  expect(o).toBe("o");
});

test("destructuring strings with rest patterns", () => {
  let word = "Hello";
  let [h, e, l1, l2, ...rest] = word;
  expect(h).toBe("H");
  expect(e).toBe("e");
  expect(l1).toBe("l");
  expect(l2).toBe("l");
  expect(rest).toEqual(["o"]);
});
