/*---
description: Compound assignment reads its target before evaluating the right-hand side
features: [compound-assignment-operators, Symbol, accessor-properties, private-fields]
---*/

// ES2026 §13.15.2 evaluates `target op= value` in a fixed order: step 3 takes
// GetValue(lRef) and only step 4 evaluates the AssignmentExpression, so a
// right-hand side that rebinds the target is folded against the value the
// target held *before* it ran.

let moduleBinding = "A";

describe("compound assignment on identifiers", () => {
  test("a function-scoped binding is read before the right-hand side", () => {
    let text = "A";

    text += ((text = "B"), "C");

    expect(text).toBe("AC");
  });

  test("the numeric result folds against the value read first", () => {
    let value = 1;

    value += ((value = 10), 100);

    expect(value).toBe(101);
  });

  test("the expression result is the stored value", () => {
    let value = 1;

    expect((value += ((value = 10), 100))).toBe(101);
    expect(value).toBe(101);
  });

  test("every arithmetic and bitwise operator reads the target first", () => {
    let subtract = 1;
    subtract -= ((subtract = 10), 100);
    expect(subtract).toBe(-99);

    let multiply = 2;
    multiply *= ((multiply = 10), 100);
    expect(multiply).toBe(200);

    let divide = 1000;
    divide /= ((divide = 10), 100);
    expect(divide).toBe(10);

    let remainder = 7;
    remainder %= ((remainder = 100), 5);
    expect(remainder).toBe(2);

    let power = 2;
    power **= ((power = 10), 3);
    expect(power).toBe(8);

    let bitAnd = 6;
    bitAnd &= ((bitAnd = 15), 3);
    expect(bitAnd).toBe(2);

    let bitOr = 4;
    bitOr |= ((bitOr = 15), 1);
    expect(bitOr).toBe(5);

    let bitXor = 6;
    bitXor ^= ((bitXor = 15), 3);
    expect(bitXor).toBe(5);

    let shiftLeft = 1;
    shiftLeft <<= ((shiftLeft = 8), 2);
    expect(shiftLeft).toBe(4);

    let shiftRight = 16;
    shiftRight >>= ((shiftRight = 1024), 2);
    expect(shiftRight).toBe(4);

    let shiftRightUnsigned = 16;
    shiftRightUnsigned >>>= ((shiftRightUnsigned = 1024), 2);
    expect(shiftRightUnsigned).toBe(4);
  });

  test("a non-numeric target rebound to a number keeps string semantics", () => {
    // The operand read in step 3 carries its own type, so the concatenation is
    // decided by the string the target held — not by the number the
    // right-hand side left behind.
    let text = "a";

    text += ((text = 5), 2);

    expect(text).toBe("a2");
    expect(typeof text).toBe("string");
    // A following expression must still see a string, not a number.
    expect(text + 1).toBe("a21");
  });

  test("a numeric-looking string target is not folded as a number", () => {
    let text = "10";

    text += ((text = 5), 2);

    expect(text).toBe("102");
    expect(typeof text).toBe("string");
  });

  test("a binding last written through a closure reads that written value", () => {
    // The write goes through the closure's view of `count`; the compound
    // assignment has to read the same binding rather than a stale copy.
    let count = 1;
    const set = (next) => {
      count = next;
    };

    set(50);
    count += 1;

    expect(count).toBe(51);
  });

  test("a string binding last written through a closure reads that value", () => {
    let text = "A";
    const set = (next) => {
      text = next;
    };

    set("Q");
    text += "C";

    expect(text).toBe("QC");
  });

  test("a target in its temporal dead zone throws before the right-hand side", () => {
    // GetValue(lRef) in step 3 fails on an uninitialized binding, so step 4
    // never evaluates the right-hand side.
    const log = [];
    const run = () => {
      dead += (log.push("rhs"), 1);
      let dead = 1;
      return dead;
    };

    expect(run).toThrow(ReferenceError);
    expect(log).toEqual([]);
  });

  test("a captured binding is read before a call that rebinds it", () => {
    let text = "A";
    const rebind = () => {
      text = "B";
      return "C";
    };

    text += rebind();

    expect(text).toBe("AC");
  });

  test("an outer binding reached as an upvalue is read first", () => {
    const run = () => {
      let text = "A";
      const compound = () => {
        text += ((text = "B"), "C");
      };

      compound();
      return text;
    };

    expect(run()).toBe("AC");
  });

  test("a module-level binding is read first", () => {
    moduleBinding = "A";

    moduleBinding += ((moduleBinding = "B"), "C");

    expect(moduleBinding).toBe("AC");
  });
});

describe("compound assignment on properties", () => {
  test("a named target is read before the right-hand side", () => {
    const object = { slot: "A" };

    object.slot += ((object.slot = "B"), "C");

    expect(object.slot).toBe("AC");
  });

  test("a computed string key is read before the right-hand side", () => {
    const object = { slot: "A" };
    const key = "slot";

    object[key] += ((object[key] = "B"), "C");

    expect(object[key]).toBe("AC");
  });

  test("a computed symbol key is read before the right-hand side", () => {
    const key = Symbol("slot");
    const object = { [key]: "A" };

    object[key] += ((object[key] = "B"), "C");

    expect(object[key]).toBe("AC");
  });

  test("an array element is read before the right-hand side", () => {
    const values = ["A"];

    values[0] += ((values[0] = "B"), "C");

    expect(values[0]).toBe("AC");
  });

  test("a private field is read before the right-hand side", () => {
    class Box {
      #slot = "A";

      compound() {
        this.#slot += ((this.#slot = "B"), "C");
        return this.#slot;
      }
    }

    expect(new Box().compound()).toBe("AC");
  });
});

describe("compound assignment ordering is observable through accessors", () => {
  test("the getter runs before the right-hand side and the setter after it", () => {
    const log = [];
    const target = {
      get slot() {
        log.push("get");
        return "A";
      },
      set slot(next) {
        log.push("set:" + next);
      },
    };

    target.slot += ((log.push("rhs"), "C"));

    expect(log).toEqual(["get", "rhs", "set:AC"]);
  });

  test("the base and the key are evaluated before the target is read", () => {
    const log = [];
    const object = {
      get slot() {
        log.push("get");
        return 1;
      },
      set slot(next) {
        log.push("set:" + next);
      },
    };
    const base = () => {
      log.push("base");
      return object;
    };
    const key = () => {
      log.push("key");
      return "slot";
    };

    base()[key()] += ((log.push("rhs"), 2));

    expect(log).toEqual(["base", "key", "get", "rhs", "set:3"]);
  });
});

describe("short-circuiting compound assignment keeps its own order", () => {
  // §13.15.2 for `??=`, `&&=` and `||=` reads the target first as well, but the
  // right-hand side decides the stored value on its own — there is nothing to
  // fold, so a rebinding right-hand side is simply overwritten by the store.
  test("??= stores the right-hand side over a rebinding side effect", () => {
    let value = null;

    value ??= ((value = 5), 7);

    expect(value).toBe(7);
  });

  test("&&= evaluates the right-hand side only for a truthy target", () => {
    let truthy = "A";
    truthy &&= ((truthy = "B"), "C");
    expect(truthy).toBe("C");

    let falsy = "";
    let evaluated = 0;
    falsy &&= ((evaluated += 1), "C");
    expect(falsy).toBe("");
    expect(evaluated).toBe(0);
  });

  test("||= evaluates the right-hand side only for a falsy target", () => {
    let falsy = "";
    falsy ||= ((falsy = "B"), "C");
    expect(falsy).toBe("C");

    let truthy = "A";
    let evaluated = 0;
    truthy ||= ((evaluated += 1), "C");
    expect(truthy).toBe("A");
    expect(evaluated).toBe(0);
  });
});
