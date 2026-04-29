/*---
description: Class generator methods
features: [generators]
---*/

test("class generator method works without compat-function", () => {
  class Counter {
    constructor(start) {
      this.start = start;
    }

    *values() {
      yield this.start;
      yield this.start + 1;
    }
  }

  const iter = new Counter(5).values();
  expect(iter.next()).toEqual({ value: 5, done: false });
  expect(iter.next()).toEqual({ value: 6, done: false });
});
