/*---
description: return inside traditional for-loop body unwinds the loop
features: [compat-traditional-for-loop]
---*/

test("return inside body unwinds", () => {
  const f = () => {
    for (let i = 0; i < 10; i++) {
      if (i === 3) return i;
    }
    return -1;
  };
  expect(f()).toBe(3);
});

test("return value reflects loop state at unwind", () => {
  const f = () => {
    let total = 0;
    for (let i = 0; i < 10; i++) {
      total += i;
      if (total > 5) return total;
    }
    return total;
  };
  expect(f()).toBe(6);
});

test("throw inside body propagates", () => {
  const f = () => {
    for (let i = 0; i < 10; i++) {
      if (i === 2) throw new Error("stop");
    }
  };
  expect(f).toThrow();
});
