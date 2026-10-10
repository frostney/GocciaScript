/*---
description: >
  A closure's write to a traditional for loop's let binding carries into the
  next iteration when a labeled continue from an inner loop ends the iteration
features: [compat-label, compat-traditional-for-loop]
---*/

test("labeled continue after a closure writes the outer loop binding", () => {
  const seen = [];
  outer: for (let i = 0; i < 6; i++) {
    const skip = () => {
      i += 1;
    };
    for (const x of [1]) {
      if (i === 1) {
        skip();
        continue outer;
      }
    }
    seen.push(i);
  }
  expect(seen).toEqual([0, 3, 4, 5]);
});

test("labeled continue after a closure writes the outer loop binding, identifier limit", () => {
  const run = (limit) => {
    const seen = [];
    outer: for (let i = 0; i < limit; i++) {
      const skip = () => {
        i += 1;
      };
      for (let j = 0; j < 1; j++) {
        if (i === 1) {
          skip();
          continue outer;
        }
      }
      seen.push(i);
    }
    return seen;
  };
  expect(run(6)).toEqual([0, 3, 4, 5]);
});
