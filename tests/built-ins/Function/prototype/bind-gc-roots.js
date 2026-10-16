/*---
description: Function.prototype.bind keeps the bound this, the bound arguments and the target reachable while the target's length and name getters run
features: [Goccia.gc, Function.prototype.bind]
---*/

const hasGoccia = typeof Goccia !== "undefined";

// A bare gc() usually leaves a freed slot readable; the allocation churn after
// it is what makes a collected temporary observable.
const churn = () => {
  Goccia.gc();
  let total = 0;
  for (const i of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
    const scratch = { a: i * 7.5, b: [i, i + 1], c: "x" + i };
    total += scratch.a + scratch.b[0];
  }
  return total;
};

// ES2026 §20.2.3.2: step 3 (BoundFunctionCreate) captures the bound this and
// arguments, and steps 5-8 only then read the target's length and name. Both
// reads are guest code here, so whatever bind is holding must survive a
// collection.
const makeTarget = () => {
  const target = ({
    m(...args) {
      return [this, ...args];
    },
  }).m;
  Object.defineProperty(target, "length", {
    get() {
      churn();
      return 5;
    },
  });
  Object.defineProperty(target, "name", {
    get() {
      churn();
      return "target";
    },
  });
  return target;
};

describe.runIf(hasGoccia)("Function.prototype.bind GC roots", () => {
  test("binding no arguments keeps the target alive", () => {
    const bound = makeTarget().bind();
    churn();
    expect(bound(1)).toEqual([undefined, 1]);
    expect(bound.name).toBe("bound target");
    expect(bound.length).toBe(5);
  });

  test("a fresh boxed number as the only argument survives as thisArg", () => {
    const target = makeTarget();
    let x = 4;
    const bound = target.bind(x + 0.5);
    churn();
    expect(bound(1)).toEqual([4.5, 1]);
    expect(bound.length).toBe(5);
  });

  test("a fresh boxed number survives as the single bound argument", () => {
    const target = makeTarget();
    const keep = {};
    let x = 4;
    const bound = target.bind(keep, x + 0.5);
    churn();
    const result = bound(1);
    expect(result[0]).toBe(keep);
    expect(result.slice(1)).toEqual([4.5, 1]);
    expect(bound.length).toBe(4);
  });

  test("fresh this and argument values of every kind survive a single-argument bind", () => {
    const target = makeTarget();
    let x = 2;
    let s = "fr";
    const bound = target.bind(x + 0.25, { tag: s + "esh" });
    churn();
    const result = bound(s + "om");
    expect(result[0]).toBe(2.25);
    expect(result[1].tag).toBe("fresh");
    expect(result[2]).toBe("from");
  });

  test("a fresh object and a fresh string survive as thisArg", () => {
    const target = makeTarget();
    let s = "th";
    const objectBound = target.bind({ tag: s + "is" });
    const stringBound = target.bind(s + "at", 7);
    churn();
    expect(objectBound()[0].tag).toBe("this");
    expect(stringBound()).toEqual(["that", 7]);
  });

  test("several fresh bound arguments survive", () => {
    const target = makeTarget();
    let x = 1;
    let s = "a";
    const bound = target.bind({ tag: s + "this" }, x + 0.5, s + "b", { tag: s + "c" }, x * 3.5);
    churn();
    const result = bound(9);
    expect(result[0].tag).toBe("athis");
    expect(result[1]).toBe(1.5);
    expect(result[2]).toBe("ab");
    expect(result[3].tag).toBe("ac");
    expect(result[4]).toBe(3.5);
    expect(result[5]).toBe(9);
    expect(bound.length).toBe(1);
    expect(bound.name).toBe("bound target");
  });

  test("bind reached through call keeps its arguments alive", () => {
    const target = makeTarget();
    let x = 6;
    const bound = Function.prototype.bind.call(target, x + 0.5, x + 0.25);
    churn();
    expect(bound()).toEqual([6.5, 6.25]);
  });

  test("a throwing length getter does not corrupt a several-argument bind", () => {
    const target = ({ m(a, b, c) { return [a, b, c]; } }).m;
    Object.defineProperty(target, "length", {
      get() {
        churn();
        throw new Error("boom");
      },
    });
    let x = 1;
    for (const i of [1, 2, 3, 4, 5]) {
      expect(() => target.bind(null, { i }, "s" + i, x + 0.5)).toThrow("boom");
    }
    churn();
    expect(churn()).toBe(467.5);
  });
});
