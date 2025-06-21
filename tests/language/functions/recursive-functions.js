/*---
description: Recursive functions work correctly
features: [recursive-functions]
---*/

test("simple recursion - factorial", () => {
  const factorial = (n) => {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
  };

  expect(factorial(0)).toBe(1);
  expect(factorial(1)).toBe(1);
  expect(factorial(5)).toBe(120);
});

test("recursion with multiple base cases", () => {
  const fibonacci = (n) => {
    if (n <= 0) return 0;
    if (n === 1) return 1;
    return fibonacci(n - 1) + fibonacci(n - 2);
  };

  expect(fibonacci(0)).toBe(0);
  expect(fibonacci(1)).toBe(1);
  expect(fibonacci(6)).toBe(8); // 0,1,1,2,3,5,8
});

test("tail recursion", () => {
  const sum = (n, acc = 0) => {
    if (n <= 0) return acc;
    return sum(n - 1, acc + n);
  };

  expect(sum(5)).toBe(15); // 1+2+3+4+5
});
