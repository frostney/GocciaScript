/*---
description: Array binding patterns of 255 or more elements bind every element
features: [destructuring]
---*/

// A binding pattern of only identifiers and holes reads the source up to a
// count held in an 8-bit operand; a longer pattern steps the iterator instead.
// A pattern of 255 or more elements, or a rest element after the 255th, used
// to stop the process with a fatal error.
const names = (prefix, count) =>
  Array.from({ length: count }, (_, i) => `${prefix}${i}`);
const range = (count) => Array.from({ length: count }, (_, i) => i);

describe("long array binding patterns", () => {
  test("a rest element after 300 bindings", () => {
    const bound = new Function("arr", `const [${names("a", 300).join(", ")}, ...rest] = arr; return [a0, a299, rest];`)(range(310));
    expect(bound[0]).toBe(0);
    expect(bound[1]).toBe(299);
    expect(bound[2]).toEqual([300, 301, 302, 303, 304, 305, 306, 307, 308, 309]);
  });

  test("255 bindings without a rest element", () => {
    const bound = new Function("arr", `const [${names("a", 255).join(", ")}] = arr; return [a0, a254];`)(range(300));
    expect(bound).toEqual([0, 254]);
  });

  test("a long pattern closes the iterator after the last binding", () => {
    let closed = 0;
    let next = 0;
    const iterable = {
      [Symbol.iterator]() {
        return {
          next: () => ({ value: next++, done: false }),
          return() {
            closed++;
            return {};
          },
        };
      },
    };
    const last = new Function("it", `const [${names("c", 300).join(", ")}] = it; return c299;`)(iterable);
    expect(last).toBe(299);
    expect(next).toBe(300);
    expect(closed).toBe(1);
  });
});
