/*---
description: If and conditional-expression tests built from &&, || and ! short-circuit and decide like the values they produce
features: [if-statement, logical-operators]
---*/

const values = [0, -0, NaN, "", 0n, null, undefined, false, 1, "a", 1n, true, {}, [], Symbol("s")];

describe("&&, || and ! as branch conditions", () => {
  test("an if takes the branch ToBoolean of the whole expression picks, evaluating operands in order", () => {
    for (const a of values) {
      for (const b of values) {
        const log = [];
        const left = () => { log.push("a"); return a; };
        const right = () => { log.push("b"); return b; };

        let taken = false;
        if (left() && right()) { taken = true; }
        expect(taken).toBe(Boolean(a && b));
        expect(log).toEqual(a ? ["a", "b"] : ["a"]);

        log.length = 0;
        taken = false;
        if (left() || right()) { taken = true; }
        expect(taken).toBe(Boolean(a || b));
        expect(log).toEqual(a ? ["a"] : ["a", "b"]);

        log.length = 0;
        taken = false;
        if (!(left() && !right())) { taken = true; }
        expect(taken).toBe(!(a && !b));

        log.length = 0;
        expect((left() || right()) && !left() ? "then" : "else")
          .toBe((a || b) && !a ? "then" : "else");
      }
    }
  });

  test("nested mixes keep their grouping", () => {
    for (const a of [false, true]) {
      for (const b of [false, true]) {
        for (const c of [false, true]) {
          let taken = false;
          if ((a || b) && !c) { taken = true; }
          expect(taken).toBe((a || b) && !c);
          taken = false;
          if (a && (b || !c)) { taken = true; }
          expect(taken).toBe(a && (b || !c));
          expect(!(a || b) || c ? 1 : 2).toBe(!(a || b) || c ? 1 : 2);
        }
      }
    }
  });

  test("a local assigned inside the condition is tested with its new value", () => {
    let x = 1;
    let taken = false;
    if ((x = 0) || x) { taken = true; }
    expect(taken).toBe(false);
    if ((x = 2) && x === 2) { taken = true; }
    expect(taken).toBe(true);
  });

  test("a local a call in the condition changes is tested after the call", () => {
    let x = true;
    const clear = () => { x = false; return true; };
    let taken = false;
    if (clear() && x) { taken = true; }
    expect(taken).toBe(false);
  });
});
