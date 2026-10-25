/*---
description: Conditions comparing a value with a small integer constant
features: [less-than, greater-than, less-than-or-equal, greater-than-or-equal, strict-equality, bigint]
---*/

// `x < k`, `x > k`, `x <= k`, `x >= k`, `x === k` and `x !== k` with k a
// small integer constant are tested in if, loop and conditional conditions.
// Each case below compares the branch taken with the operator's own value.

const relations = [
  ["<", (x, k) => x < k],
  [">", (x, k) => x > k],
  ["<=", (x, k) => x <= k],
  [">=", (x, k) => x >= k],
];

test("if conditions with a constant on the right match the operator", () => {
  const values = [-40000, -32768, -300, -1, -0, 0, 0.5, 1, 4, 5, 6, 255,
    256, 300, 32767, 40000, NaN, Infinity, -Infinity];
  for (const x of values) {
    let taken = [];
    if (x < 5) { taken.push("<"); }
    if (x > 5) { taken.push(">"); }
    if (x <= 5) { taken.push("<="); }
    if (x >= 5) { taken.push(">="); }
    if (x === 5) { taken.push("==="); }
    if (x !== 5) { taken.push("!=="); }
    const expected = [];
    if ((x < 5) === true) { expected.push("<"); }
    if ((x > 5) === true) { expected.push(">"); }
    if ((x <= 5) === true) { expected.push("<="); }
    if ((x >= 5) === true) { expected.push(">="); }
    if ((x === 5) === true) { expected.push("==="); }
    if ((x !== 5) === true) { expected.push("!=="); }
    expect(taken).toEqual(expected);
  }
});

test("negative, zero and wide constants compare as Numbers", () => {
  const check = (x) => [
    x < -300 ? 1 : 0, x > -300 ? 1 : 0, x <= -1 ? 1 : 0, x >= 0 ? 1 : 0,
    x < 300 ? 1 : 0, x > 32767 ? 1 : 0, x <= -32768 ? 1 : 0,
    x === 0 ? 1 : 0, x !== 300 ? 1 : 0, x === -300 ? 1 : 0,
  ];
  expect(check(-300)).toEqual([0, 0, 1, 0, 1, 0, 0, 0, 1, 1]);
  expect(check(-0)).toEqual([0, 1, 0, 1, 1, 0, 0, 1, 1, 0]);
  expect(check(300)).toEqual([0, 1, 0, 1, 0, 0, 0, 0, 0, 0]);
  expect(check(-32768)).toEqual([1, 0, 1, 0, 1, 0, 1, 0, 1, 0]);
  expect(check(40000)).toEqual([0, 1, 0, 1, 0, 1, 0, 0, 1, 0]);
  expect(check(NaN)).toEqual([0, 0, 0, 0, 0, 0, 0, 0, 1, 0]);
});

test("a constant on the left is compared the other way round", () => {
  const values = [-1, 4, 5, 6, NaN, "5", "6", null, undefined, true];
  for (const x of values) {
    expect(5 < x ? "yes" : "no").toBe((x > 5) ? "yes" : "no");
    expect(5 > x ? "yes" : "no").toBe((x < 5) ? "yes" : "no");
    expect(5 <= x ? "yes" : "no").toBe((x >= 5) ? "yes" : "no");
    expect(5 >= x ? "yes" : "no").toBe((x <= 5) ? "yes" : "no");
    expect(5 === x ? "yes" : "no").toBe((x === 5) ? "yes" : "no");
    expect(5 !== x ? "yes" : "no").toBe((x !== 5) ? "yes" : "no");
  }
});

test("values that are not Numbers keep the operators' conversions", () => {
  const cases = ["4", "5", "6", "", "abc", " 5 ", null, undefined, true,
    false, [], [6], {}, 4n, 5n, 6n];
  for (const x of cases) {
    for (const [name, relation] of relations) {
      let taken = false;
      if (name === "<") {
        if (x < 5) { taken = true; }
      } else if (name === ">") {
        if (x > 5) { taken = true; }
      } else if (name === "<=") {
        if (x <= 5) { taken = true; }
      } else if (x >= 5) {
        taken = true;
      }
      expect(taken).toBe(relation(x, 5));
    }
  }
});

test("strict equality with a constant never converts", () => {
  let calls = 0;
  const object = {
    valueOf() {
      calls++;
      return 5;
    },
  };
  const values = ["5", 5n, true, null, undefined, object, [5], new Number(5)];
  for (const x of values) {
    expect(x === 5 ? "eq" : "ne").toBe("ne");
    expect(x !== 5 ? "ne" : "eq").toBe("ne");
  }
  expect(calls).toBe(0);
  expect(5 === 5.0 ? "eq" : "ne").toBe("eq");
  const zero = -0;
  expect(zero === 0 ? "eq" : "ne").toBe("eq");
});

test("ToPrimitive runs once and its errors propagate", () => {
  const order = [];
  const value = {
    valueOf() {
      order.push("valueOf");
      return 7;
    },
  };
  if (value > 5) {
    order.push("then");
  }
  if (5 < value) {
    order.push("then");
  }
  if (value <= 5) {
    order.push("else");
  }
  expect(order).toEqual(["valueOf", "then", "valueOf", "then", "valueOf"]);

  const throwing = {
    [Symbol.toPrimitive]() {
      throw new RangeError("no primitive");
    },
  };
  expect(() => {
    if (throwing < 5) {
      return 1;
    }
    return 0;
  }).toThrow(RangeError);
  expect(() => (Symbol("s") >= 5 ? 1 : 0)).toThrow(TypeError);
  expect(Symbol.iterator === 5 ? 1 : 0).toBe(0);
});

test("logical chains use the constant comparison", () => {
  let a = 1;
  let b = 2;
  expect(a === 1 && b === 2 ? "both" : "not").toBe("both");
  expect(a === 2 || b === 2 ? "either" : "neither").toBe("either");
  expect(a !== 1 || b !== 3 ? "either" : "neither").toBe("either");
  expect(a > 0 && b < 3 ? "in" : "out").toBe("in");
  expect(a > 1 || b >= 2 ? "in" : "out").toBe("in");
  expect(!(NaN < 5) ? "not less" : "less").toBe("not less");
  expect(!(NaN >= 5) ? "not ge" : "ge").toBe("not ge");
});

test("a const binding is used as the constant", () => {
  const LIMIT = 10;
  const NEGATIVE = -2;
  const take = (x) => (x < LIMIT ? "below" : "not below") + "," +
    (x === NEGATIVE ? "neg" : "other");
  expect(take(3)).toBe("below,other");
  expect(take(-2)).toBe("below,neg");
  expect(take(10)).toBe("not below,other");
});

test("a const read before its declaration still throws", () => {
  const before = (x) => {
    if (x < LATER) {
      return "lt";
    }
    return "ge";
  };
  expect(() => before(1)).toThrow(ReferenceError);
  {
    expect(() => {
      if (1 === BLOCKED) {
        return 1;
      }
      return 0;
    }).toThrow(ReferenceError);
    const BLOCKED = 1;
  }
  const LATER = 5;
  expect(before(1)).toBe("lt");
  expect(before(9)).toBe("ge");
});
