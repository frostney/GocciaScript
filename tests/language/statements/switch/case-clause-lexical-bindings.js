/*---
description: let and const bindings declared directly in case clauses keep their TDZ and fall-through values, nested switches included
features: [switch-statement]
---*/

// All case clauses of a switch share one scope (ES2026 §14.12.2, the
// CaseBlock's LexicallyScopedDeclarations), and a clause can be entered
// without running the clauses before it. Each binding is in its TDZ until its
// own declaration runs, whichever clause the switch starts at.

test("let bindings keep their values across fall-through and their TDZ on direct entry", () => {
  const run = (which) => {
    const seen = [];
    switch (which) {
      case 0:
        let a = 1;
        a += 1;
        seen.push(a);
      case 1:
        let b = 10;
        seen.push(b);
      case 2:
        seen.push(typeof b === "number" ? b + a : "unreachable");
        break;
      case 3:
        let d = 4;
        seen.push(d);
    }
    return seen;
  };

  expect(run(0)).toEqual([2, 10, 12]);
  expect(() => run(1)).toThrow(ReferenceError);
  expect(() => run(2)).toThrow(ReferenceError);
  expect(run(3)).toEqual([4]);
});

test("a constant declared in a clause is not folded into a clause entered directly", () => {
  const read = (which) => {
    switch (which) {
      case 0:
        const k = 7;
        const twice = k * 2;
      case 1:
        return twice + 1;
    }
  };

  expect(read(0)).toBe(15);
  expect(() => read(1)).toThrow(ReferenceError);
});

test("a nested switch keeps the outer clauses' TDZ and its own", () => {
  const read = (outer, inner) => {
    switch (outer) {
      case 0:
        const a = 1;
        switch (inner) {
          case 0:
            const b = 2;
            return a + b;
          case 1:
            return b;
          case 2:
            const c = 3;
        }
        const after = a + 10;
      case 1:
        return after;
      case 2:
        return a;
    }
  };

  expect(read(0, 0)).toBe(3);
  expect(() => read(0, 1)).toThrow(ReferenceError);
  expect(read(0, 2)).toBe(11);
  expect(read(0, 3)).toBe(11);
  expect(() => read(1, 0)).toThrow(ReferenceError);
  expect(() => read(2, 0)).toThrow(ReferenceError);
});

test("a binding of an inner block in a clause does not leak into a later clause", () => {
  const read = (which) => {
    const label = "outer";
    switch (which) {
      case 0: {
        const label = "inner";
        if (label !== "inner") return "wrong";
      }
      case 1:
        return label;
    }
  };

  expect(read(0)).toBe("outer");
  expect(read(1)).toBe("outer");
});
