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
});
