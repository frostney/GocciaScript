/*---
description: Compound addition and subtraction of a small integer literal
features: [compound-assignment]
---*/

test("x += k and x -= k on Numbers give the Number result", () => {
  let count = 0;
  count += 1;
  count += 300;
  count -= 2;
  count += -40000;
  expect(count).toBe(-39701);

  let fraction = 0.5;
  fraction += 1;
  fraction -= 3;
  expect(fraction).toBe(-1.5);
});

test("special Number values keep their arithmetic", () => {
  let infinite = Infinity;
  infinite -= 1;
  expect(infinite).toBe(Infinity);
  let notANumber = NaN;
  notANumber += 1;
  expect(Number.isNaN(notANumber)).toBe(true);
  let negativeZero = -0;
  negativeZero += 0;
  expect(Object.is(negativeZero, 0)).toBe(true);
  let large = 2 ** 53;
  large += 1;
  expect(large).toBe(2 ** 53);
});

test("a binding a closure can rebind keeps the generic operator", () => {
  // Under --strict-types the rebinding itself is a TypeError and the binding
  // keeps its Number.
  let value = 1;
  let rebound = true;
  const rebind = () => {
    value = "text";
  };
  try {
    rebind();
  } catch (error) {
    rebound = !(error instanceof TypeError);
  }
  value += 1;
  expect(value).toBe(rebound ? "text1" : 2);
});

test("Strings and BigInts keep the generic operator", () => {
  let text = "a";
  text += 1;
  expect(text).toBe("a1");
  let big = 5n;
  expect(() => {
    big += 1;
  }).toThrow(TypeError);
  expect(big).toBe(5n);
});

test("a binding updated in a loop counts every step", () => {
  const steps = [1, 2, 3, 4];
  let total = 0;
  for (const step of steps) {
    total += 2;
    total -= 1;
    total += step;
  }
  expect(total).toBe(14);
});

test("an annotated binding that was never assigned gives NaN", () => {
  let declared: number;
  declared += 1;
  expect(Number.isNaN(declared)).toBe(true);
  let other: number;
  other -= 1;
  expect(Number.isNaN(other)).toBe(true);
});
