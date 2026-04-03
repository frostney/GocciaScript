/*---
description: String.prototype.search with regex arguments
features: [String.prototype.search]
---*/

test("search returns the first match index", () => {
  expect("zabcq".search(/abc/)).toBe(1);
  expect("hello".search(/l+/)).toBe(2);
});

test("search returns -1 when no match exists", () => {
  expect("hello".search(/z+/)).toBe(-1);
});

test("search dispatches through Symbol.search", () => {
  const searcher = {
    [Symbol.search](input) {
      expect(input).toBe("abc");
      return 7;
    },
  };

  expect("abc".search(searcher)).toBe(7);
});
