/*---
description: The bindings a loop body declares are in their TDZ on every iteration until declared
features: [let, const, arrow-function, for-of]
---*/

const outcome = (read) => {
  try {
    return read();
  } catch (error) {
    return error.constructor.name;
  }
};

describe("let and const at the start of a loop body", () => {
  test("a closure from an earlier initializer, called from a later one, throws on every iteration", () => {
    const seen = [];
    for (const step of [1, 2, 3]) {
      const read = () => later;
      const later = [outcome(read), step];
      seen.push(later[0]);
    }
    expect(seen).toEqual(["ReferenceError", "ReferenceError", "ReferenceError"]);
  });

  test("a closure created by an earlier iteration reads that iteration's binding", () => {
    const readers = [];
    let previous = null;
    for (const step of [1, 2, 3]) {
      const fromBefore = previous ? previous() : 0;
      const value = step * 10 + fromBefore;
      previous = () => value;
      readers.push(previous);
    }
    expect(readers.map((read) => read())).toEqual([10, 30, 60]);
  });

  test("a read in a statement before the declaration throws on every iteration", () => {
    const seen = [];
    for (const step of [1, 2]) {
      seen.push(outcome(() => value));
      const value = step;
      seen.push(value);
    }
    expect(seen).toEqual(["ReferenceError", 1, "ReferenceError", 2]);
  });

  test("an inner block's leading bindings are fresh on each entry", () => {
    const results = [];
    for (const step of [1, 2, 3]) {
      {
        const doubled = step * 2;
        const read = () => doubled;
        results.push(read);
      }
    }
    expect(results.map((read) => read())).toEqual([2, 4, 6]);
  });

  test("a body left by continue and break declares its bindings again", () => {
    const seen = [];
    for (const step of [1, 2, 3, 4]) {
      const read = () => current;
      const current = outcome(read) === "ReferenceError" ? step : -1;
      if (current === 2) {
        continue;
      }
      seen.push(current);
      if (current === 3) {
        break;
      }
    }
    expect(seen).toEqual([1, 3]);
  });
});
