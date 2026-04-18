describe('Object.prototype.hasOwnProperty', () => {
  test('returns true for own property', () => {
    const obj = { x: 1, y: 2 };
    expect(obj.hasOwnProperty('x')).toBe(true);
    expect(obj.hasOwnProperty('y')).toBe(true);
  });

  test('returns false for non-existent property', () => {
    const obj = { x: 1 };
    expect(obj.hasOwnProperty('z')).toBe(false);
    expect(obj.hasOwnProperty('toString')).toBe(false);
  });

  test('returns false for inherited properties', () => {
    class Base {
      baseMethod() { return 1; }
    }
    class Child extends Base {
      childProp = 42;
    }
    const c = new Child();
    expect(c.hasOwnProperty('childProp')).toBe(true);
    expect(c.hasOwnProperty('baseMethod')).toBe(false);
  });

  test('works with string keys that are numbers', () => {
    const obj = { 0: 'a', 1: 'b' };
    expect(obj.hasOwnProperty('0')).toBe(true);
    expect(obj.hasOwnProperty('2')).toBe(false);
  });

  test('returns true for properties with undefined value', () => {
    const obj = { x: undefined };
    expect(obj.hasOwnProperty('x')).toBe(true);
  });

  test('returns true for properties with null value', () => {
    const obj = { x: null };
    expect(obj.hasOwnProperty('x')).toBe(true);
  });

  test('called with no arguments checks "undefined" key', () => {
    const obj = { undefined: 1 };
    expect(obj.hasOwnProperty()).toBe(true);

    const obj2 = { x: 1 };
    expect(obj2.hasOwnProperty()).toBe(false);
  });

  test('coerces argument to string', () => {
    const obj = { 1: 'one', true: 'yes', null: 'nil' };
    expect(obj.hasOwnProperty(1)).toBe(true);
    expect(obj.hasOwnProperty(true)).toBe(true);
    expect(obj.hasOwnProperty(null)).toBe(true);
  });

  test('works on arrays', () => {
    const arr = [10, 20, 30];
    expect(arr.hasOwnProperty('0')).toBe(true);
    expect(arr.hasOwnProperty('2')).toBe(true);
    expect(arr.hasOwnProperty('3')).toBe(false);
    expect(arr.hasOwnProperty('length')).toBe(true);
  });

  test('distinguishes own from prototype chain', () => {
    const parent = { shared: true };
    const child = Object.create(parent);
    child.own = true;

    expect(child.hasOwnProperty('own')).toBe(true);
    expect(child.hasOwnProperty('shared')).toBe(false);
  });

  test('works on class instances', () => {
    class Foo {
      constructor() {
        this.a = 1;
      }
      method() { return 2; }
    }
    const f = new Foo();
    expect(f.hasOwnProperty('a')).toBe(true);
    expect(f.hasOwnProperty('method')).toBe(false);
  });

  test('works with symbol keys', () => {
    const sym = Symbol("own");
    const other = Symbol("missing");
    const obj = { [sym]: "value" };
    expect(obj.hasOwnProperty(sym)).toBe(true);
    expect(obj.hasOwnProperty(other)).toBe(false);
  });

  test('works with well-known symbol keys', () => {
    const obj = { [Symbol.toStringTag]: "Custom" };
    expect(obj.hasOwnProperty(Symbol.toStringTag)).toBe(true);
    expect(obj.hasOwnProperty(Symbol.iterator)).toBe(false);
  });

  // §20.1.3.2 vs §20.1.2.9 — behavioral differences from Object.hasOwn
  // hasOwnProperty is a legacy API; Object.hasOwn is the modern replacement.
  // These tests document the cases where the two diverge.

  test('overridden hasOwnProperty takes precedence', () => {
    const obj = {
      hasOwnProperty: () => false,
      bar: "exists"
    };
    // Legacy behavior: the override wins, returning a wrong answer
    expect(obj.hasOwnProperty("bar")).toBe(false);
    // Modern API bypasses the override
    expect(Object.hasOwn(obj, "bar")).toBe(true);
  });

  test('not available on null-prototype objects', () => {
    const obj = Object.create(null);
    obj.prop = "exists";
    // Legacy behavior: null-prototype objects lack hasOwnProperty
    expect(() => obj.hasOwnProperty("prop")).toThrow(TypeError);
    // Modern API works regardless of prototype chain
    expect(Object.hasOwn(obj, "prop")).toBe(true);
  });

  test('Object.prototype.hasOwnProperty.call bypasses override', () => {
    const obj = {
      hasOwnProperty: () => false,
      key: 1
    };
    // Defensive call-pattern avoids the override issue
    expect(Object.prototype.hasOwnProperty.call(obj, "key")).toBe(true);
    expect(Object.prototype.hasOwnProperty.call(obj, "missing")).toBe(false);
  });

  test('Object.prototype.hasOwnProperty.call works on null-prototype objects', () => {
    const obj = Object.create(null);
    obj.a = 1;
    // The call-pattern also works for null-prototype objects
    expect(Object.prototype.hasOwnProperty.call(obj, "a")).toBe(true);
    expect(Object.prototype.hasOwnProperty.call(obj, "b")).toBe(false);
  });
});
