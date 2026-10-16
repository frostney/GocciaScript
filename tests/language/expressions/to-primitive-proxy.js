/*---
description: A callable Proxy works as Symbol.toPrimitive, valueOf or toString in every conversion to a primitive
features: [Proxy, Symbol.toPrimitive]
---*/

// ES2026 §7.1.1 ToPrimitive step 1.a reads @@toPrimitive with GetMethod, which
// asks only IsCallable (§7.3.10 step 3), and step 1.b.iv calls it. A Proxy whose
// target is a function has [[Call]] (§10.5.12), so it qualifies. So does a
// valueOf or toString method, which OrdinaryToPrimitive (§7.1.1.1 step 3.b)
// calls when IsCallable is true. Expected values match Node.js 24.

const hinted = () => ({ [Symbol.toPrimitive]: new Proxy((hint) => "h:" + hint, {}) });

describe("a callable Proxy as Symbol.toPrimitive", () => {
  test("unary plus and Number() call it with the number hint", () => {
    expect(+{ [Symbol.toPrimitive]: new Proxy(() => 1, {}) }).toBe(1);
    expect(Number({ [Symbol.toPrimitive]: new Proxy(() => 3, {}) })).toBe(3);
    expect(+{ [Symbol.toPrimitive]: new Proxy((hint) => (hint === "number" ? 7 : 0), {}) }).toBe(7);
  });

  test("template literals, String() and + pass the string and default hints", () => {
    expect(`${hinted()}`).toBe("h:string");
    expect(String(hinted())).toBe("h:string");
    expect(hinted() + "").toBe("h:default");
  });

  test("a computed property key is converted through it", () => {
    expect(({ a: 1 })[{ [Symbol.toPrimitive]: new Proxy(() => "a", {}) }]).toBe(1);
  });

  test("the Proxy's apply trap runs with the target, the receiver and the hint", () => {
    const calls = [];
    const target = () => 1;
    const object = {
      [Symbol.toPrimitive]: new Proxy(target, {
        apply: (t, receiver, args) => {
          calls.push([t === target, receiver === object, args[0]]);
          return 41 + t();
        },
      }),
    };
    expect(+object).toBe(42);
    expect(calls).toEqual([[true, true, "number"]]);
  });

  test("an error thrown inside it propagates as an ordinary exception", () => {
    const object = { [Symbol.toPrimitive]: new Proxy(() => { throw new SyntaxError("from the proxy"); }, {}) };
    expect(() => +object).toThrow(SyntaxError);
  });

  test("an object result throws TypeError", () => {
    expect(() => +{ [Symbol.toPrimitive]: new Proxy(() => ({}), {}) }).toThrow(TypeError);
  });

  test("a class passes GetMethod but throws TypeError when called", () => {
    expect(() => +{ [Symbol.toPrimitive]: class {} }).toThrow(TypeError);
  });
});

describe("a callable Proxy as valueOf or toString", () => {
  test("valueOf is called for the number hint", () => {
    expect(+{ valueOf: new Proxy(() => 2, {}) }).toBe(2);
    expect(Math.max({ valueOf: new Proxy(() => 9, {}) }, 1)).toBe(9);
  });

  test("toString is called for the string hint", () => {
    expect(`${{ toString: new Proxy(() => "ts", {}) }}`).toBe("ts");
    expect(String({ toString: new Proxy(() => "es", {}) })).toBe("es");
    expect(String.raw({ raw: { length: 1, 0: { toString: new Proxy(() => "raw", {}) } } })).toBe("raw");
  });

  test("valueOf is called with the object as this", () => {
    const object = { n: 5, valueOf: new Proxy(({ m() { return this.n; } }).m, {}) };
    expect(object * 2).toBe(10);
  });

  test("an error thrown inside valueOf propagates as an ordinary exception", () => {
    expect(() => +{ valueOf: new Proxy(() => { throw new SyntaxError("x"); }, {}) }).toThrow(SyntaxError);
  });

  test("a class as valueOf throws TypeError", () => {
    expect(() => +{ valueOf: class {} }).toThrow(TypeError);
  });
});
