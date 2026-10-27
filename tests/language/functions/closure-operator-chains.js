/*---
description: Closures that read many captured bindings in one chain of operators capture the bindings they name
features: [let, const, arrow-functions, closures, for-of]
---*/

const opaque = (value) => value;

test("each closure sums the bindings of its own function", () => {
  const make = (p) => {
    let a = p + 1;
    let b = p + 2;
    let c = p + 3;
    let d = p + 4;
    let e = p + 5;
    let f = p + 6;
    const all = () => a + b + c + d + e + f;
    const firstHalf = () => a + b + c;
    const reversed = () => f + e + d + c + b + a;
    const twice = () => a + a + b + b;
    return [all, firstHalf, reversed, twice];
  };
  const [all, firstHalf, reversed, twice] = make(opaque(10));
  expect(all()).toBe(81);
  expect(firstHalf()).toBe(36);
  expect(reversed()).toBe(81);
  expect(twice()).toBe(46);
});

test("a closure sees a later assignment to a binding it sums", () => {
  let a = 1;
  let b = 2;
  let c = 3;
  const sum = () => a + b + c;
  expect(sum()).toBe(6);
  b = "x";
  expect(sum()).toBe("1x3");
  c = 10;
  expect(sum()).toBe("1x10");
});

test("a closure assigning a binding is seen by a closure summing it", () => {
  let a = 1;
  let b = 2;
  const sum = () => a + b + 1;
  const write = (value) => {
    b = value;
  };
  expect(sum()).toBe(4);
  write("s");
  expect(sum()).toBe("1s1");
});

test("a shadowing binding is the one the chain reads", () => {
  const outer = (p) => {
    let a = p;
    let b = p * 10;
    const inner = (q) => {
      let a = q;
      const sum = () => a + b + a;
      return sum;
    };
    const outerSum = () => a + b + a;
    return [inner(opaque(7))(), outerSum()];
  };
  expect(outer(opaque(1))).toEqual([24, 12]);
});

test("a block's binding shadows the function's in the closures created in it", () => {
  let n = 1;
  let m = 2;
  const closures = [];
  closures.push(() => n + m + n);
  {
    let n = 100;
    closures.push(() => n + m + n);
  }
  closures.push(() => m + n + m);
  expect(closures.map((f) => f())).toEqual([4, 202, 5]);
});

test("closures created in a loop keep their iteration's bindings", () => {
  let base = 1000;
  const closures = [];
  for (const i of [1, 2, 3]) {
    const j = i * 10;
    let k = i * 100;
    closures.push(() => base + k + j + i);
    k = k + 1;
  }
  base = 0;
  expect(closures.map((f) => f())).toEqual([112, 223, 334]);
});

test("nested functions capture through every level", () => {
  const level1 = (a) => {
    let b = a + 1;
    return (c) => {
      let d = c + 1;
      return (e) => a + b + c + d + e + a + d;
    };
  };
  expect(level1(opaque(1))(opaque(10))(opaque(100))).toBe(136);
});

test("a closure summing many captured bindings", () => {
  const make = (p) => {
    let x0 = p + 0; let x1 = p + 1; let x2 = p + 2; let x3 = p + 3;
    let x4 = p + 4; let x5 = p + 5; let x6 = p + 6; let x7 = p + 7;
    let x8 = p + 8; let x9 = p + 9; let x10 = p + 10; let x11 = p + 11;
    let x12 = p + 12; let x13 = p + 13; let x14 = p + 14; let x15 = p + 15;
    const sumAll = () => x0 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 +
      x10 + x11 + x12 + x13 + x14 + x15;
    const sumEven = () => x0 + x2 + x4 + x6 + x8 + x10 + x12 + x14;
    const sumOdd = () => x15 + x13 + x11 + x9 + x7 + x5 + x3 + x1;
    const update = () => {
      x7 = "seven";
    };
    return [sumAll, sumEven, sumOdd, update];
  };
  const [sumAll, sumEven, sumOdd, update] = make(opaque(1));
  expect(sumAll()).toBe(136);
  expect(sumEven()).toBe(64);
  expect(sumOdd()).toBe(72);
  update();
  expect(sumAll()).toBe("28seven910111213141516");
  expect(sumOdd()).toBe("52seven642");
  expect(sumEven()).toBe(64);
});
