/*---
description: A callable Proxy called as a method keeps its receiver reachable while the apply-trap lookup runs guest code
features: [Goccia.gc, Proxy, Symbol]
---*/

const hasGoccia = typeof Goccia !== "undefined";

// A bare gc() usually leaves a freed slot readable; the allocation churn after
// it is what makes a collected receiver observable.
const churn = () => {
  Goccia.gc();
  let total = 0;
  for (const i of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
    const scratch = { v: i * 7.5, tag: "x" + i, list: [i, i + 1] };
    total += scratch.v + scratch.list[0];
  }
  return total;
};

// Reading `apply` off the handler is guest code that runs before the Proxy
// forwards the call, so the receiver must already be rooted by then.
const churningHandler = () => ({
  get apply() {
    churn();
    return undefined;
  },
});

describe.runIf(hasGoccia)("callable Proxy receiver GC roots", () => {
  test("Symbol.toPrimitive on a fresh object reads its receiver after the trap lookup", () => {
    const methods = {
      toPrimitive() {
        return this.v + this.tag.length;
      },
    };
    const toPrimitive = new Proxy(methods.toPrimitive, churningHandler());
    const results = [1, 2, 3, 4, 5].map(
      (i) => ({ v: i * 10.5, tag: "abc", [Symbol.toPrimitive]: toPrimitive }) + 1,
    );
    expect(results).toEqual([14.5, 25, 35.5, 46, 56.5]);
  });

  test("valueOf on a fresh object reads its receiver after the trap lookup", () => {
    const methods = {
      valueOf() {
        return this.v * 2;
      },
    };
    const valueOf = new Proxy(methods.valueOf, churningHandler());
    const results = [1, 2, 3].map((i) => ({ v: i + 0.25, valueOf }) * 1);
    expect(results).toEqual([2.5, 4.5, 6.5]);
  });
});
