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

test("Symbol.search fires before ToString on the receiver (ES2026 §22.1.3.27 step 2)", () => {
  let poisoned = false;
  let searcherFired = false;
  const poison = { toString() { poisoned = true; return ""; } };
  const searcher = {
    [Symbol.search]() { searcherFired = true; return -1; },
  };
  "".search.call(poison, searcher);
  expect(searcherFired).toBe(true);
  expect(poisoned).toBe(false);
});

test("Symbol.search receives O directly, not a stringified copy", () => {
  const receiver = { tag: "unique-receiver" };
  let receivedThis;
  const searcher = {
    [Symbol.search](o) { receivedThis = o; return -1; },
  };
  "".search.call(receiver, searcher);
  expect(receivedThis).toBe(receiver);
});

test("search throws TypeError when called on null or undefined", () => {
  expect(() => "".search.call(null, /x/)).toThrow(TypeError);
  expect(() => "".search.call(undefined, /x/)).toThrow(TypeError);
});
