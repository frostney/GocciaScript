/*---
description: Class async generator methods
features: [async-generators]
---*/

test("class async generator method works without compat-function", async () => {
  class Counter {
    constructor(start) {
      this.start = start;
    }

    async *values() {
      yield await Promise.resolve(this.start);
      yield this.start + 1;
    }
  }

  const iter = new Counter(7).values();
  expect(await iter.next()).toEqual({ value: 7, done: false });
  expect(await iter.next()).toEqual({ value: 8, done: false });
});
