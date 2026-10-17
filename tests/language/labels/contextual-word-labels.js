/*---
description: Contextual words are LabelIdentifiers (ES2026 §13.1, §14.13); let and static only outside strict mode code
features: [compat-label, compat-non-strict-mode, unsafe-function-constructor]
---*/

describe("contextual words as labels", () => {
  test("as labels a block and a for...of loop", () => {
    let n = 0;
    as: for (const x of [1, 2, 3]) {
      if (x === 2) continue as;
      n += x;
      if (x === 3) break as;
    }
    as: {
      n += 10;
      break as;
      n += 100;
    }
    expect(n).toBe(14);
  });

  test("from labels a block and a for...of loop", () => {
    let n = 0;
    from: for (const x of [1, 2, 3]) {
      if (x === 2) continue from;
      n += x;
      if (x === 3) break from;
    }
    from: {
      n += 10;
      break from;
      n += 100;
    }
    expect(n).toBe(14);
  });

  test("async labels a block and a for...of loop", () => {
    let n = 0;
    async: for (const x of [1, 2, 3]) {
      if (x === 2) continue async;
      n += x;
      if (x === 3) break async;
    }
    async: {
      n += 10;
      break async;
      n += 100;
    }
    expect(n).toBe(14);
  });

  test("let and static label statements in sloppy code", () => {
    let n = 0;
    let: for (const x of [1, 2, 3]) {
      if (x === 2) continue let;
      n += x;
    }
    static: {
      n += 10;
      break static;
      n += 100;
    }
    expect(n).toBe(14);
  });

  test("as, from and async are labels in strict code too", () => {
    const run = new Function(`"use strict";
      let n = 0;
      as: for (const x of [1, 2]) { from: { async: { n += x; break from; } } continue as; }
      return n;`);
    expect(run()).toBe(3);
  });

  test("let and static are not labels in strict code", () => {
    expect(() => new Function('"use strict"; let: {}')).toThrow(SyntaxError);
    expect(() => new Function('"use strict"; static: {}')).toThrow(SyntaxError);
  });

  test("enum is never a label", () => {
    expect(() => new Function("enum: {}")).toThrow(SyntaxError);
  });

  test("async still starts async functions and arrows", () => {
    const af = new Function(`
      async function f() { return 1; }
      const g = async (x) => x + 1;
      return [f, g];`);
    const [f, g] = af();
    expect(f() instanceof Promise).toBe(true);
    expect(g(1) instanceof Promise).toBe(true);
  });
});
