/*---
description: An operand that names a let binding or a parameter keeps the value it had when it was evaluated, whatever a later operand does to the binding
features: [let, arrow-functions, default-parameters, destructuring, Symbol.toPrimitive, Proxy]
---*/

// ES2026 §13.15.3 and its siblings evaluate the left operand and take its
// value (GetValue) before the right operand is evaluated. A right operand that
// rebinds the same variable therefore must not change what the operator sees
// on the left. Every helper below takes its inputs as arguments so that
// nothing can be folded at compile time.

const opaque = (value) => value;

describe("binary operators read a parameter before a later operand rebinds it", () => {
  test("assignment in the right operand", () => {
    const add = (a) => a + (a = 5);
    const multiply = (a) => a * (a = 5);
    const subtract = (a) => a - (a = 5);

    expect(add(opaque(1))).toBe(6);
    expect(multiply(opaque(3))).toBe(15);
    expect(subtract(opaque(9))).toBe(4);
  });

  test("assignment in the left operand is seen by the right one", () => {
    const add = (a) => (a = 5) + a;
    const multiply = (a) => (a = 4) * a;

    expect(add(opaque(1))).toBe(10);
    expect(multiply(opaque(1))).toBe(16);
  });

  test("increment and decrement in the right operand", () => {
    const postIncrement = (a) => a + a++;
    const preIncrement = (a) => a * ++a;
    const postDecrement = (a) => a - a--;
    const preDecrement = (a) => a + --a;

    expect(postIncrement(opaque(1))).toBe(2);
    expect(preIncrement(opaque(3))).toBe(12);
    expect(postDecrement(opaque(7))).toBe(0);
    expect(preDecrement(opaque(7))).toBe(13);
  });

  test("compound and logical assignment in the right operand", () => {
    const compound = (a) => a + (a += 10);
    const nullish = (a, b) => b + ((b ??= 7), a);
    const logicalOr = (a) => a + (a ||= 9);
    const logicalAnd = (a) => a * (a &&= 9);

    expect(compound(opaque(1))).toBe(12);
    expect(nullish(opaque(2), opaque(3))).toBe(5);
    expect(logicalOr(opaque(0))).toBe(9);
    expect(logicalAnd(opaque(2))).toBe(18);
  });

  test("destructuring assignment in the right operand", () => {
    const swap = (a, b) => a + (([a, b] = [b, a]), a) * 10 + b * 100;
    const fromObject = (a) => a + (({ a } = { a: 40 }), a);

    expect(swap(opaque(1), opaque(2))).toBe(121);
    expect(fromObject(opaque(2))).toBe(42);
  });

  test("a rebinding buried in a nested operand", () => {
    const nested = (a, b) => a + (b + (a = 9)) + a;
    const conditional = (a, flag) => a + (flag ? (a = 20) : 0) + a;
    const sequence = (a) => a + ((a = 2), (a = 3), a);
    const template = (a) => a + `${(a = 8)}`;

    expect(nested(opaque(2), opaque(3))).toBe(23);
    expect(conditional(opaque(1), true)).toBe(41);
    expect(conditional(opaque(1), false)).toBe(2);
    expect(sequence(opaque(1))).toBe(4);
    expect(template(opaque(1))).toBe("18");
  });

  test("a rebinding buried in each kind of expression", () => {
    const identity = (value) => value;
    const ignore = () => 0;
    const tag = (strings, value) => value;
    class Box {
      #secret = 1;
      constructor(value) {
        this.value = value;
      }
      read(a) {
        return a + ((a = 50), this).#secret + a;
      }
      write(a) {
        return a + (this.#secret = a = 50) + a;
      }
      writeObject(a) {
        return a + (((a = 50), this).#secret = 2) + a;
      }
      compound(a) {
        return a + (this.#secret += a = 50) + a;
      }
      compoundObject(a) {
        return a + (((a = 50), this).#secret += 2) + a;
      }
      destructure(a) {
        return a + ([((a = 50), this).#secret] = [2])[0] + a;
      }
    }
    const cases = {
      binary: (a) => a + (1 + (a = 50)) + a,
      relational: (a) => a + ((a = 50) > 1 ? 1 : 0) + a,
      membership: (a, o) => a + ("x" in ((a = 50), o) ? 1 : 0) + a,
      unary: (a) => a + -(a = 50) + a,
      typeOf: (a) => a + (typeof (a = 50)).length + a,
      logicalAnd: (a) => a + (a && (a = 50)) + a,
      logicalOr: (a) => a + (0 || (a = 50)) + a,
      nullish: (a) => a + (null ?? (a = 50)) + a,
      conditionTest: (a) => a + ((a = 50) ? 1 : 2) + a,
      conditionThen: (a) => a + (a ? (a = 50) : 2) + a,
      conditionElse: (a) => a + (a ? 1 : 2) + (!a ? 1 : (a = 50)) + a,
      sequence: (a) => a + (0, (a = 50)) + a,
      callArgument: (a) => a + identity((a = 50)) + a,
      callCallee: (a) => a + ((a = 50), identity)(1) + a,
      callSpread: (a) => a + identity(...[(a = 50)]) + a,
      construct: (a) => a + new Box((a = 50)).value + a,
      memberObject: (a, o) => a + ((a = 50), o).x + a,
      memberKey: (a, o) => a + o[((a = 50), "x")] + a,
      optionalKey: (a, o) => a + o?.[((a = 50), "x")] + a,
      optionalCall: (a, o) => a + o.f?.((a = 50)) + a,
      storeValue: (a, o) => a + (o.x = a = 50) + a,
      storeObject: (a, o) => a + (((a = 50), o).x = 1) + a,
      storeKey: (a, o) => a + (o[((a = 50), "x")] = 1) + a,
      storeElementValue: (a, o) => a + (o["x"] = a = 50) + a,
      compoundStore: (a, o) => a + (o.x += a = 50) + a,
      compoundElementStore: (a, o) => a + (o[((a = 50), "x")] += 1) + a,
      compoundStoreObject: (a, o) => a + (((a = 50), o).x += 1) + a,
      compoundElementStoreObject: (a, o) => a + (((a = 50), o)["x"] += 1) + a,
      compoundElementStoreValue: (a, o) => a + (o["x"] += a = 50) + a,
      memberIncrement: (a, o) => a + o[((a = 50), "x")]++ + a,
      arrayLiteral: (a) => a + [(a = 50)][0] + a,
      arraySpread: (a) => a + [...[(a = 50)]][0] + a,
      objectLiteral: (a) => a + { k: (a = 50) }.k + a,
      objectComputedKey: (a) => a + { [((a = 50), "k")]: 1 }.k + a,
      objectSpread: (a) => a + { ...{ k: (a = 50) } }.k + a,
      template: (a) => a + `${(a = 50)}`.length + a,
      taggedTemplate: (a) => a + tag`x${(a = 50)}` + a,
      taggedTemplateTag: (a) => a + ((a = 50), tag)`x${1}` + a,
      destructuringArray: (a) => a + ([a] = [50])[0] + a,
      destructuringObject: (a) => a + ({ k: a } = { k: 50 }).k + a,
      destructuringDefault: (a, o) => a + ([o.y = a = 50] = [])[0] + a,
      importSpecifier: (a) => a + (import(((a = 50), "./missing-module.js")).catch(ignore) ? 1 : 0) + a,
      importOptions: (a) => a + (import("./missing-module.js", ((a = 50), {})).catch(ignore) ? 1 : 0) + a,
      privateRead: (a) => new Box(0).read(a),
      privateWrite: (a) => new Box(0).write(a),
      privateWriteObject: (a) => new Box(0).writeObject(a),
      privateCompound: (a) => new Box(0).compound(a),
      privateCompoundObject: (a) => new Box(0).compoundObject(a),
      privateDestructure: (a) => new Box(0).destructure(a),
    };
    const results = {};
    for (const name of Object.keys(cases)) {
      results[name] = cases[name](opaque(1), { x: 1, f: identity });
    }

    expect(results).toEqual({
      binary: 102,
      relational: 52,
      membership: 52,
      unary: 1,
      typeOf: 57,
      logicalAnd: 101,
      logicalOr: 101,
      nullish: 101,
      conditionTest: 52,
      conditionThen: 101,
      conditionElse: 102,
      sequence: 101,
      callArgument: 101,
      callCallee: 52,
      callSpread: 101,
      construct: 101,
      memberObject: 52,
      memberKey: 52,
      optionalKey: 52,
      optionalCall: 101,
      storeValue: 101,
      storeObject: 52,
      storeKey: 52,
      storeElementValue: 101,
      compoundStore: 102,
      compoundElementStore: 53,
      compoundStoreObject: 53,
      compoundElementStoreObject: 53,
      compoundElementStoreValue: 102,
      memberIncrement: 52,
      arrayLiteral: 101,
      arraySpread: 101,
      objectLiteral: 101,
      objectComputedKey: 52,
      objectSpread: 101,
      template: 53,
      taggedTemplate: 101,
      taggedTemplateTag: 52,
      destructuringArray: 101,
      destructuringObject: 101,
      destructuringDefault: NaN,
      importSpecifier: 52,
      importOptions: 52,
      privateRead: 52,
      privateWrite: 101,
      privateWriteObject: 53,
      privateCompound: 102,
      privateCompoundObject: 54,
      privateDestructure: 53,
    });
  });

  test("every operand of a longer chain keeps its own value", () => {
    const chain = (a) => a + (a = a * 2) + (a = a * 2) + a;

    expect(chain(opaque(1))).toBe(11);
  });

  test("the fused number-and-literal forms", () => {
    const minusLiteral = (n) => n - 1 + ((n = 10), n) - 1;
    const plusLiteral = (n) => n + 1 + ((n = 10), n) + 1;
    const literalFirst = (n) => 1 + n + ((n = 10), 1 + n);

    expect(minusLiteral(5)).toBe(13);
    expect(plusLiteral(5)).toBe(17);
    expect(literalFirst(5)).toBe(17);
  });
});

describe("binary operators read a let binding before a later operand rebinds it", () => {
  test("assignment, increment and compound assignment", () => {
    const run = (seed) => {
      let value = seed;
      const assigned = value * (value = 7);
      const incremented = value + value++;
      const compounded = value - (value -= 3);
      return [assigned, incremented, compounded, value];
    };

    expect(run(opaque(2))).toEqual([14, 14, 3, 5]);
  });

  test("two bindings rebinding each other", () => {
    const run = (first, second) => {
      let left = first;
      let right = second;
      const total = left + (right = left + right) + (left = right) + left + right;
      return [total, left, right];
    };

    expect(run(opaque(1), opaque(2))).toEqual([13, 3, 3]);
  });

  test("a binding declared without an initializer", () => {
    const run = (seed) => {
      let pending;
      const before = pending + 1;
      pending = seed;
      return [Number.isNaN(before), pending + 1, pending + (pending = 4)];
    };

    expect(run(opaque(2))).toEqual([true, 3, 6]);
  });

  test("the binding of a for...of loop", () => {
    const run = (items) => {
      const seen = [];
      for (let item of items) {
        item = item + (item = 10);
        seen.push(item + (item += 1), item);
      }
      return seen;
    };

    expect(run(opaque([1, 2]))).toEqual([23, 12, 25, 13]);
  });

  test("a binding the compiler knows to hold a number", () => {
    const run = (replacement) => {
      let count = 5;
      const minus = count - 1 + ((count = replacement), count) - 1;
      count = 5;
      const plus = count + 1 + ((count = replacement), count) + 1;
      count = 5;
      const compare = count <= 5 ? ((count = replacement), count) : -1;
      return [minus, plus, compare];
    };

    expect(run(opaque(10))).toEqual([13, 17, 10]);
  });

  test("an inner block shadows the outer binding only inside the block", () => {
    const run = (seed) => {
      let value = seed;
      let inner = 0;
      {
        let value = seed * 10;
        inner = value + (value = 1) + value;
      }
      return [inner, value + (value = 5) + value];
    };

    expect(run(opaque(2))).toEqual([22, 12]);
  });
});

describe("comparisons read their operands in order", () => {
  test("less-than in a conditional expression", () => {
    const left = (a, b) => (a < ((a = 100), b) ? "less" : "not less");
    const right = (a, b) => (((b = -100), a) < b ? "less" : "not less");

    expect(left(opaque(1), opaque(2))).toBe("less");
    expect(right(opaque(1), opaque(2))).toBe("not less");
  });

  test("less-than in an if statement", () => {
    const run = (a, b) => {
      if (a < ((a = 100), b)) {
        return ["less", a];
      }
      return ["not less", a];
    };

    expect(run(opaque(1), opaque(2))).toEqual(["less", 100]);
    expect(run(opaque(3), opaque(2))).toEqual(["not less", 100]);
  });

  test("less-than-or-equal against a literal", () => {
    const clamp = (n) => (n <= 1 ? n : n - 1);
    const rebound = (n) => (n <= 1 ? ((n = 50), n) : ((n = 60), n));

    expect([clamp(0), clamp(1), clamp(2), clamp(9)]).toEqual([0, 1, 1, 8]);
    expect([rebound(1), rebound(2)]).toEqual([50, 60]);
  });

  test("the remaining relational and equality operators", () => {
    const run = (a) => [
      a > ((a = 0), 1),
      a >= ((a = 5), 5),
      a <= ((a = 9), 4),
      a === ((a = 1), 9),
      a !== ((a = 2), 1),
    ];

    expect(run(opaque(3))).toEqual([true, false, false, true, false]);
  });
});

describe("property and element access read their operands in order", () => {
  test("the object is read before a computed key rebinds it", () => {
    const run = (target, other) => {
      const value = target[((target = other), "name")];
      return [value, target.name];
    };

    expect(run(opaque({ name: "first" }), opaque({ name: "second" }))).toEqual(["first", "second"]);
  });

  test("the key is read when it is evaluated", () => {
    const run = (items, index) => items[index] + items[(index = index + 1)] + items[index];

    expect(run(opaque([1, 20, 300]), opaque(0))).toBe(41);
  });

  test("a property store takes its object before the value rebinds it", () => {
    const run = (target, other) => {
      const original = target;
      target.value = ((target = other), 10);
      return [original.value, other.value];
    };

    expect(run(opaque({ value: 1 }), opaque({ value: 2 }))).toEqual([10, 2]);
  });

  test("an element store takes its object and key before the value rebinds them", () => {
    const run = (target, other, index) => {
      const original = target;
      target[index] = ((target = other), (index = 1), 5);
      return [original.join(), other.join(), index];
    };

    expect(run(opaque([0, 0]), opaque([0, 0]), opaque(0))).toEqual(["5,0", "0,0", 1]);
  });

  test("an element store takes its object before the key rebinds it", () => {
    const run = (target, other) => {
      const original = target;
      target[((target = other), 0)] = 7;
      return [original.join(), other.join()];
    };

    expect(run(opaque([0, 0]), opaque([0, 0]))).toEqual(["7,0", "0,0"]);
  });

  test("the stored value is the value the binding had when it was evaluated", () => {
    const run = (target, value) => {
      target.first = value;
      value = value + 1;
      target.second = value;
      target[value] = value;
      return [target.first, target.second, target[value]];
    };

    expect(run(opaque({}), opaque(1))).toEqual([1, 2, 2]);
  });
});

describe("a call in a later operand cannot change an operand already read", () => {
  test("a closure created before the read", () => {
    const run = (a) => {
      const bump = () => {
        a = a + 100;
        return 1;
      };
      return [a + bump(), a + bump() + a, a];
    };

    expect(run(opaque(1))).toEqual([2, 303, 201]);
  });

  test("valueOf, toString and Symbol.toPrimitive that rebind the operand", () => {
    const run = (a) => {
      const byValueOf = { valueOf: () => ((a = a + 10), 1) };
      const byToString = { toString: () => ((a = a + 20), "2") };
      const byPrimitive = { [Symbol.toPrimitive]: () => ((a = a + 30), 3) };
      return [a + byValueOf, a + byToString, a + byPrimitive, a * byValueOf, a];
    };

    expect(run(opaque(1))).toEqual([2, "112", 34, 61, 71]);
  });

  test("a getter and a proxy trap that rebind the operand", () => {
    const run = (a) => {
      const holder = {
        get bump() {
          a = a + 10;
          return 1;
        },
      };
      const proxy = new Proxy(
        {},
        {
          get: (target, key) => {
            a = a + 100;
            return key === "x" ? 2 : undefined;
          },
        },
      );
      return [a + holder.bump, a + proxy.x, a];
    };

    expect(run(opaque(1))).toEqual([2, 13, 111]);
  });

  test("a default parameter value that assigns an earlier parameter", () => {
    const run = (a, b = (a = 9)) => a + b;
    const readsFirst = (a, b = a + (a = 9)) => [a, b];

    expect(run(opaque(3))).toBe(18);
    expect(run(opaque(3), opaque(1))).toBe(4);
    expect(readsFirst(opaque(3))).toEqual([9, 12]);
  });

  test("an argument list evaluated after the operand", () => {
    const sum = (...values) => values.reduce((total, value) => total + value, 0);
    const run = (a) => a + sum((a = 5), a, (a = 6), a) + a;

    expect(run(opaque(1))).toBe(29);
  });
});
