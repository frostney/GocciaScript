/*---
description: While conditions comparing a counter with a small integer constant
features: [compat-while-loops]
---*/

test("a while condition compares its counter with a constant", () => {
  let count = 0;
  let i = 0;
  while (i < 300) {
    i += 1;
    count += 1;
  }
  expect(count).toBe(300);

  let z = 0;
  let notDone = true;
  while (notDone && z < 50) {
    if (z * 2 > 40) {
      notDone = false;
    }
    z += 1;
  }
  expect(z).toBe(22);
});

test("a while condition with a non-Number operand keeps its conversions", () => {
  const order = [];
  let remaining = 3;
  const counter = {
    valueOf() {
      order.push(remaining);
      remaining -= 1;
      return remaining;
    },
  };
  while (counter >= 0) {
    order.push("body");
  }
  expect(order).toEqual([3, "body", 2, "body", 1, "body", 0]);

  let text = "a";
  while (text.length < 4 && text !== 5) {
    text += "a";
  }
  expect(text).toBe("aaaa");
});
