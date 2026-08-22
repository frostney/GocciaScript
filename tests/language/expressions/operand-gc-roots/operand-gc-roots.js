/*---
description: binary operator operands survive collections forced from their own coercion hooks
features: [Goccia.gc, compat-loose-equality, Symbol.toPrimitive, Proxy]
---*/

// A binary operator materializes both operands before handing them to the
// shared Goccia.Arithmetic helpers, and the helpers then coerce each operand in
// turn through ToPrimitive — which re-enters guest code whenever an operand is
// an object with valueOf / toString / Symbol.toPrimitive.
//
// Materializing is an allocation in the bytecode VM: a register holding an
// integer other than 0/1, or any float, becomes a fresh number object that
// lives only in a native temporary. Nothing else refers to it, so a collection
// driven from the *other* operand's hook could sweep it and hand the freed
// block straight back to the hook's own allocation. The operator then read the
// wrong operand and returned a wrong number rather than failing — `box * 2`
// answered 25 instead of 10, and `box * 3` and `box * 7` both answered 25 too,
// because both were really computing `5 * 5`.
//
// Every case below therefore asserts an exact value with a distinctive result,
// and collects twice inside the hook so a surviving operand has to be genuinely
// rooted rather than merely not-yet-swept. The operand pairs deliberately avoid
// 0 and 1, whose number objects are pinned singletons that never allocate.

const collect = () => {
  Goccia.gc();
  Goccia.gc();
};

// valueOf hook: collects, then returns a value that itself needs an allocation.
const box = (n) => ({
  valueOf() {
    collect();
    return n;
  },
});

// valueOf hook that allocates a pile of garbage first, so the collection has
// something to reclaim and the freed block is very likely to be reused.
const churningBox = (n) => ({
  valueOf() {
    const junk = Array.from({ length: 256 }, (_, i) => ({ i, s: `${i}` }));
    collect();
    return n + junk.length - 256;
  },
});

const stringBox = (s) => ({
  toString() {
    collect();
    return s;
  },
});

const primitiveBox = (n) => ({
  [Symbol.toPrimitive]() {
    collect();
    return n;
  },
});

describe("operator operands under collection", () => {
  test("multiplication keeps the other operand alive", () => {
    // The clincher: if the surviving operand were the hook's own value these
    // would all answer 25.
    expect(box(5) * 2).toBe(10);
    expect(box(5) * 3).toBe(15);
    expect(box(5) * 7).toBe(35);
    expect(2 * box(5)).toBe(10);
    expect(7 * box(5)).toBe(35);
    // 0 and 1 are the pinned singletons; they must keep working too.
    expect(box(5) * 0).toBe(0);
    expect(box(5) * 1).toBe(5);
  });

  test("addition keeps the other operand alive", () => {
    expect(box(5) + 2).toBe(7);
    expect(2 + box(5)).toBe(7);
    expect(box(5) + 7).toBe(12);
  });

  test("subtraction keeps the other operand alive", () => {
    expect(box(5) - 2).toBe(3);
    expect(2 - box(5)).toBe(-3);
    expect(box(9) - 4).toBe(5);
  });

  test("division keeps the other operand alive", () => {
    expect(box(5) / 2).toBe(2.5);
    expect(20 / box(8)).toBe(2.5);
  });

  test("remainder keeps the other operand alive", () => {
    expect(box(5) % 3).toBe(2);
    expect(23 % box(5)).toBe(3);
  });

  test("exponentiation keeps the other operand alive", () => {
    expect(box(5) ** 2).toBe(25);
    expect(2 ** box(5)).toBe(32);
  });

  test("float operands survive too", () => {
    expect(box(5) * 2.5).toBe(12.5);
    expect(box(2.5) * 4).toBe(10);
    expect(box(2.5) - 0.5).toBe(2);
  });

  test("relational comparisons keep the other operand alive", () => {
    expect(box(5) < 9).toBe(true);
    expect(box(5) > 9).toBe(false);
    expect(box(5) <= 4).toBe(false);
    expect(box(5) >= 4).toBe(true);
    expect(9 > box(5)).toBe(true);
    expect(4 >= box(5)).toBe(false);
  });

  test("loose equality keeps the other operand alive", () => {
    expect(box(5) == 5).toBe(true);
    expect(box(5) == 7).toBe(false);
    expect(7 == box(5)).toBe(false);
    expect(box(5) != 7).toBe(true);
    expect(box(5) == "5").toBe(true);
  });

  test("bitwise and shift operators keep the other operand alive", () => {
    expect(box(5) & 12).toBe(4);
    expect(box(5) | 8).toBe(13);
    expect(box(5) ^ 12).toBe(9);
    expect(box(5) << 3).toBe(40);
    expect(box(40) >> 2).toBe(10);
    expect(box(40) >>> 2).toBe(10);
    expect(12 & box(5)).toBe(4);
    expect(3 << box(4)).toBe(48);
  });

  test("both operands coercing keeps the left result alive", () => {
    // The left operand's coerced value is itself a fresh allocation, and the
    // right operand's hook collects after it exists: 9 * 4 answered 16 when it
    // was unrooted, because the left had been replaced by the right.
    expect(box(9) * box(4)).toBe(36);
    expect(box(9) - box(4)).toBe(5);
    expect(box(9) + box(4)).toBe(13);
    expect(box(9) / box(4)).toBe(2.25);
    expect(box(9) < box(4)).toBe(false);
    expect(box(4) < box(9)).toBe(true);
    expect(box(9) == box(9)).toBe(false); // distinct objects, ES2026 §7.2.13
  });

  test("hooks that allocate before collecting", () => {
    expect(churningBox(5) * 3).toBe(15);
    expect(churningBox(9) - churningBox(4)).toBe(5);
    expect(3 * churningBox(7)).toBe(21);
    expect(churningBox(5) < 9).toBe(true);
  });

  test("toString-only hooks", () => {
    expect(stringBox("8") * 4).toBe(32);
    expect(4 * stringBox("8")).toBe(32);
    expect(stringBox("8") - 3).toBe(5);
    expect("a" + stringBox("8") + "b").toBe("a8b");
    expect(stringBox("8") + 3).toBe("83");
  });

  test("Symbol.toPrimitive hooks", () => {
    expect(primitiveBox(6) * 4).toBe(24);
    expect(4 * primitiveBox(6)).toBe(24);
    expect(primitiveBox(6) < 100).toBe(true);
    expect(primitiveBox(6) - primitiveBox(2)).toBe(4);
  });

  test("compound assignment goes through the same opcodes", () => {
    let x = 3;
    x *= box(7);
    expect(x).toBe(21);

    let y = 20;
    y -= box(6);
    expect(y).toBe(14);

    let z = 5;
    z **= box(3);
    expect(z).toBe(125);

    let w = 12;
    w &= box(5);
    expect(w).toBe(4);
  });

  test("nested operators inside a coercion hook", () => {
    // The inner operator roots its own operands while the outer operator's are
    // still live; a non-LIFO rooting scheme would drop the outer pair here.
    const nesting = {
      valueOf() {
        expect(box(9) * 3).toBe(27);
        collect();
        return 5;
      },
    };
    expect(nesting * 7).toBe(35);
    expect(nesting - 2).toBe(3);
  });

  test("throwing out of a coercion hook leaves the operand rooted", () => {
    const thrower = {
      valueOf() {
        collect();
        throw new Error("from valueOf");
      },
    };
    let caught = null;
    try {
      // eslint-disable-next-line no-unused-expressions
      thrower * 7;
    } catch (e) {
      caught = e;
    }
    collect();
    expect(caught.message).toBe("from valueOf");
    // The rooting must have unwound; a leaked root would keep growing here.
    expect(box(5) * 7).toBe(35);
  });

  test("a Proxy get trap that collects still yields the right operand", () => {
    const proxied = new Proxy(
      {
        valueOf() {
          collect();
          return 5;
        },
      },
      {
        get(target, key, receiver) {
          collect();
          return Reflect.get(target, key, receiver);
        },
      },
    );
    expect(proxied * 7).toBe(35);
    expect(proxied < 9).toBe(true);
  });
});

collect();
