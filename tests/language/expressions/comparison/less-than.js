/*---
description: Less than operator (<)
features: [less-than, bigint]
---*/

test("less than operator (<)", () => {
  expect(5 < 5).toBeFalsy();
  expect(5 < 4).toBeFalsy();
  expect(5 < 6).toBeTruthy();
});

test("if condition uses less-than without changing ToPrimitive order", () => {
  const order = [];
  const left = {
    valueOf() {
      order.push("left");
      return 1;
    },
  };
  const right = {
    valueOf() {
      order.push("right");
      return 2;
    },
  };
  let tookThen = false;
  if (left < right) {
    tookThen = true;
  }
  expect(tookThen).toBe(true);
  expect(order).toEqual(["left", "right"]);
});

test("ternary less-than keeps BigInt comparison", () => {
  expect(1n < 2n ? "yes" : "no").toBe("yes");
  expect(2n < 1n ? "yes" : "no").toBe("no");
});
