describe("Object.setPrototypeOf", () => {
  test("sets the prototype of an object", () => {
    const proto = { greet: () => "hello" };
    const obj = {};
    Object.setPrototypeOf(obj, proto);
    expect(obj.greet()).toBe("hello");
  });

  test("returns the object", () => {
    const obj = {};
    const result = Object.setPrototypeOf(obj, {});
    expect(result).toBe(obj);
  });

  test("setting prototype to null", () => {
    const obj = {};
    Object.setPrototypeOf(obj, null);
    expect(Object.getPrototypeOf(obj)).toBe(null);
  });

  test("returns primitive unchanged for non-null/undefined non-object", () => {
    expect(Object.setPrototypeOf(42, null)).toBe(42);
    expect(Object.setPrototypeOf(true, null)).toBe(true);
    expect(Object.setPrototypeOf("str", null)).toBe("str");
  });

  test("throws for null first argument", () => {
    expect(() => Object.setPrototypeOf(null, null)).toThrow(TypeError);
  });

  test("throws for undefined first argument", () => {
    expect(() => Object.setPrototypeOf(undefined, null)).toThrow(TypeError);
  });

  test("throws TypeError when proto is not Object or null", () => {
    expect(() => Object.setPrototypeOf({}, 1)).toThrow(TypeError);
    expect(() => Object.setPrototypeOf({}, "str")).toThrow(TypeError);
    expect(() => Object.setPrototypeOf({}, true)).toThrow(TypeError);
  });

  test("throws TypeError on direct cycle", () => {
    const a = {};
    const b = {};
    Object.setPrototypeOf(a, b);
    expect(() => Object.setPrototypeOf(b, a)).toThrow(TypeError);
  });

  test("throws TypeError on self-cycle", () => {
    const o = {};
    expect(() => Object.setPrototypeOf(o, o)).toThrow(TypeError);
  });

  test("throws TypeError on indirect cycle through chain", () => {
    const a = {};
    const b = {};
    const c = {};
    Object.setPrototypeOf(b, a);
    Object.setPrototypeOf(c, b);
    expect(() => Object.setPrototypeOf(a, c)).toThrow(TypeError);
  });

  test("does not throw when chain converges without cycling back", () => {
    const proto = {};
    const a = Object.create(proto);
    const b = Object.create(proto);
    Object.setPrototypeOf(a, b);
    expect(Object.getPrototypeOf(a)).toBe(b);
  });

  test("setting prototype to current value is a no-op", () => {
    const proto = { greet: () => "hello" };
    const obj = Object.create(proto);
    Object.setPrototypeOf(obj, proto);
    expect(Object.getPrototypeOf(obj)).toBe(proto);
    expect(obj.greet()).toBe("hello");
  });

  test("setting prototype to null when already null is a no-op", () => {
    const obj = Object.create(null);
    Object.setPrototypeOf(obj, null);
    expect(Object.getPrototypeOf(obj)).toBe(null);
  });

  test("does not throw when proposed prototype's chain does not include target", () => {
    const a = {};
    const b = Object.create({});
    Object.setPrototypeOf(a, b);
    expect(Object.getPrototypeOf(a)).toBe(b);
  });
});
