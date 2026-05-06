/*---
description: Reflect.construct propagates newTarget through native constructors (ES2026 §10.1.13 OrdinaryCreateFromConstructor)
features: [Reflect]
---*/

describe("Reflect.construct with native Error constructors", () => {
  test("Error instance gets newTarget.prototype", () => {
    class Custom {}
    Custom.prototype.tag = "custom";
    const err = Reflect.construct(Error, ["msg"], Custom);
    expect(Object.getPrototypeOf(err)).toBe(Custom.prototype);
    expect(err.message).toBe("msg");
  });

  test("TypeError instance gets newTarget.prototype", () => {
    class Custom {}
    const te = Reflect.construct(TypeError, ["bad"], Custom);
    expect(Object.getPrototypeOf(te)).toBe(Custom.prototype);
    expect(te.message).toBe("bad");
  });

  test("ReferenceError instance gets newTarget.prototype", () => {
    class Custom {}
    const re = Reflect.construct(ReferenceError, ["ref"], Custom);
    expect(Object.getPrototypeOf(re)).toBe(Custom.prototype);
    expect(re.message).toBe("ref");
  });

  test("RangeError instance gets newTarget.prototype", () => {
    class Custom {}
    const re = Reflect.construct(RangeError, ["range"], Custom);
    expect(Object.getPrototypeOf(re)).toBe(Custom.prototype);
    expect(re.message).toBe("range");
  });

  test("SyntaxError instance gets newTarget.prototype", () => {
    class Custom {}
    const se = Reflect.construct(SyntaxError, ["syn"], Custom);
    expect(Object.getPrototypeOf(se)).toBe(Custom.prototype);
    expect(se.message).toBe("syn");
  });

  test("URIError instance gets newTarget.prototype", () => {
    class Custom {}
    const ue = Reflect.construct(URIError, ["uri"], Custom);
    expect(Object.getPrototypeOf(ue)).toBe(Custom.prototype);
    expect(ue.message).toBe("uri");
  });

  test("AggregateError instance gets newTarget.prototype", () => {
    class Custom {}
    const ae = Reflect.construct(AggregateError, [[], "agg"], Custom);
    expect(Object.getPrototypeOf(ae)).toBe(Custom.prototype);
    expect(ae.message).toBe("agg");
  });

  test("Error with cause option preserves cause under newTarget", () => {
    class Custom {}
    const cause = new Error("root");
    const err = Reflect.construct(Error, ["wrapped", { cause }], Custom);
    expect(Object.getPrototypeOf(err)).toBe(Custom.prototype);
    expect(err.cause).toBe(cause);
  });

  test("Error with newTarget === Error preserves Error.prototype", () => {
    const err = Reflect.construct(Error, ["msg"], Error);
    expect(Object.getPrototypeOf(err)).toBe(Error.prototype);
  });

  // Non-object newTarget.prototype fallback cannot be tested with classes
  // because GocciaScript's class .prototype is non-writable; the intrinsic
  // fallback path is exercised by GetProtoFromConstructorWithIntrinsic.
});

describe("Reflect.construct with native Promise constructor", () => {
  test("Promise instance gets newTarget.prototype", () => {
    class Custom {}
    Custom.prototype.tag = "custom";
    const p = Reflect.construct(Promise, [() => {}], Custom);
    expect(Object.getPrototypeOf(p)).toBe(Custom.prototype);
  });

  test("Promise executor still runs under custom newTarget", () => {
    class Custom {}
    let executorRan = false;
    Reflect.construct(Promise, [(resolve) => { executorRan = true; resolve(); }], Custom);
    expect(executorRan).toBe(true);
  });

  test("Promise with newTarget === Promise preserves Promise.prototype", () => {
    const p = Reflect.construct(Promise, [() => {}], Promise);
    expect(p instanceof Promise).toBe(true);
  });
});

describe("Reflect.construct with native RegExp constructor", () => {
  test("RegExp instance gets newTarget.prototype", () => {
    class Custom {}
    Custom.prototype.tag = "custom";
    const re = Reflect.construct(RegExp, ["abc", "g"], Custom);
    expect(Object.getPrototypeOf(re)).toBe(Custom.prototype);
  });

  test("RegExp with newTarget === RegExp preserves RegExp.prototype", () => {
    const re = Reflect.construct(RegExp, ["abc"], RegExp);
    expect(re instanceof RegExp).toBe(true);
  });
});

describe("Reflect.construct with native ArrayBuffer constructor", () => {
  test("ArrayBuffer instance gets newTarget.prototype", () => {
    class Custom {}
    Custom.prototype.tag = "custom";
    const ab = Reflect.construct(ArrayBuffer, [8], Custom);
    expect(Object.getPrototypeOf(ab)).toBe(Custom.prototype);
  });

  test("ArrayBuffer with newTarget === ArrayBuffer preserves ArrayBuffer.prototype", () => {
    const ab = Reflect.construct(ArrayBuffer, [8], ArrayBuffer);
    expect(ab instanceof ArrayBuffer).toBe(true);
  });
});

describe("Reflect.construct with Proxy forwarding newTarget", () => {
  test("Proxy with no construct trap forwards newTarget to native Error", () => {
    class Custom {}
    Custom.prototype.tag = "custom";
    const proxy = new Proxy(Error, {});
    const err = Reflect.construct(proxy, ["msg"], Custom);
    expect(Object.getPrototypeOf(err)).toBe(Custom.prototype);
    expect(err.message).toBe("msg");
  });

  test("Proxy construct trap receives newTarget and forwards it", () => {
    class Custom {}
    Custom.prototype.tag = "custom";
    let receivedNewTarget;
    const proxy = new Proxy(Error, {
      construct(target, args, newTarget) {
        receivedNewTarget = newTarget;
        return Reflect.construct(target, args, newTarget);
      },
    });
    const err = Reflect.construct(proxy, ["msg"], Custom);
    expect(receivedNewTarget).toBe(Custom);
    expect(Object.getPrototypeOf(err)).toBe(Custom.prototype);
  });

  test("new Proxy(Error, {})() uses proxy itself as default newTarget", () => {
    const proxy = new Proxy(Error, {});
    const err = new proxy("msg");
    expect(err.message).toBe("msg");
  });
});

describe("normal new still works after newTarget plumbing", () => {
  test("new Error preserves Error.prototype", () => {
    const err = new Error("test");
    expect(Object.getPrototypeOf(err)).toBe(Error.prototype);
    expect(err.message).toBe("test");
  });

  test("new Promise preserves Promise.prototype", () => {
    const p = new Promise(() => {});
    expect(p instanceof Promise).toBe(true);
  });

  test("new RegExp preserves RegExp.prototype", () => {
    const re = new RegExp("abc");
    expect(re instanceof RegExp).toBe(true);
  });

  test("new ArrayBuffer preserves ArrayBuffer.prototype", () => {
    const ab = new ArrayBuffer(8);
    expect(ab instanceof ArrayBuffer).toBe(true);
  });
});
