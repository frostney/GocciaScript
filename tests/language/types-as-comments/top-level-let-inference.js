/*---
description: Top-level lets take an inferred type only from a literal initializer under strict types
features: [types-as-comments, strict-type-enforcement]
---*/

const INFERENCE_BASE = 16;
let literalCount = 1;
let derivedCount = INFERENCE_BASE + 1;

derivedCount = 5;
const derivedSeen = [];
for (const step of [0, 1]) {
  derivedSeen.push(derivedCount + 1, derivedCount * 2, derivedCount < 5);
  derivedCount = "4";
}

describe("top-level let inference", () => {
  test("expression initializer remains untyped", () => {
    expect(derivedSeen).toEqual([6, 10, false, "41", 8, true]);
    expect(derivedCount).toBe("4");
  });

  test("functions assign another type to the untyped binding", () => {
    const assign = (next) => { derivedCount = next; };
    assign(true);
    expect(derivedCount).toBe(true);
  });

  test("literal initializer infers a type", () => {
    expect(() => { literalCount = "text"; }).toThrow(TypeError);
    expect(literalCount).toBe(1);
  });
});
