/*---
description: Growing string accumulators preserve aliases, coercion order, UTF-16 contents, and GC reachability
features: [string-concatenation, Symbol.toPrimitive]
---*/

test("growing strings preserve earlier aliases across reads and collection", () => {
  for (const initialLength of [255, 256, 257]) {
    const initial = "x".repeat(initialLength);
    let accumulated = initial;
    const chunks = [initial];
    const aliases = [];
    for (const index of Array.from({ length: 96 }, (_, i) => i)) {
      const suffix = "\uD83D\uDE80\uD800" + index + ";";
      accumulated = accumulated + suffix;
      chunks.push(suffix);
      if ([0, 30, 31, 32, 33, 63, 64, 95].includes(index)) {
        const saved = accumulated;
        aliases.push({ value: saved, read: () => saved, expected: chunks.join("") });
      }
    }
    if (typeof Goccia !== "undefined") Goccia.gc();
    for (const alias of aliases.toReversed()) {
      expect(alias.value).toBe(alias.expected);
      expect(alias.read()).toBe(alias.expected);
    }
    const expected = chunks.join("");
    expect(accumulated).toBe(expected);
    expect(accumulated.length).toBe(expected.length);
    expect(accumulated.charCodeAt(initialLength)).toBe(0xD83D);
    expect(accumulated.charCodeAt(initialLength + 2)).toBe(0xD800);
    expect(accumulated.slice(initialLength)).toBe(expected.slice(initialLength));
    expect(JSON.parse(JSON.stringify(accumulated))).toBe(expected);
    expect({ [accumulated]: 42 }[expected]).toBe(42);
    expect(accumulated + "").toBe(expected);
    expect(initial).toBe("x".repeat(initialLength));
  }
});

test("growing strings preserve primitive and object coercion", () => {
  const prefix = " ".repeat(256);
  let numeric = prefix;
  numeric += "-";
  numeric += 0;
  expect(Object.is(Number(numeric), -0)).toBe(true);
  expect(Boolean(numeric)).toBe(true);

  const events = [];
  const left = {
    [Symbol.toPrimitive](hint) {
      events.push("left:" + hint);
      if (typeof Goccia !== "undefined") Goccia.gc();
      return prefix + "left";
    },
  };
  const right = {
    [Symbol.toPrimitive](hint) {
      events.push("right:" + hint);
      if (typeof Goccia !== "undefined") Goccia.gc();
      return "right";
    },
  };
  expect(left + right).toBe(prefix + "leftright");
  expect(events).toEqual(["left:default", "right:default"]);
  expect(prefix + 12 + true + null + undefined).toBe(
    prefix + "12truenullundefined",
  );
  expect(() => prefix + Symbol("suffix")).toThrow(TypeError);
});
