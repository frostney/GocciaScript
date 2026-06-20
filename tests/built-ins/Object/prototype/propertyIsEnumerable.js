describe('Object.prototype.propertyIsEnumerable', () => {
  test('coerces property key before rejecting null receiver', () => {
    let called = false;
    const key = {
      [Symbol.toPrimitive]() {
        called = true;
        throw new Error('key conversion first');
      },
    };

    expect(() => Object.prototype.propertyIsEnumerable.call(null, key)).toThrow('key conversion first');
    expect(called).toBe(true);
  });

  test('returns true for own enumerable properties', () => {
    const obj = { x: 1, y: 2 };
    expect(obj.propertyIsEnumerable('x')).toBe(true);
    expect(obj.propertyIsEnumerable('y')).toBe(true);
  });

  test('returns false for non-existent properties', () => {
    const obj = { x: 1 };
    expect(obj.propertyIsEnumerable('z')).toBe(false);
  });

  test('returns false for inherited properties', () => {
    const parent = { inherited: true };
    const child = Object.create(parent);
    expect(child.propertyIsEnumerable('inherited')).toBe(false);
  });

  test('returns false for non-enumerable own properties', () => {
    const obj = {};
    Object.defineProperty(obj, 'hidden', {
      value: 42,
      enumerable: false,
    });
    expect(obj.propertyIsEnumerable('hidden')).toBe(false);
    expect(obj.hasOwnProperty('hidden')).toBe(true);
  });

  test('returns true for enumerable defined properties', () => {
    const obj = {};
    Object.defineProperty(obj, 'visible', {
      value: 42,
      enumerable: true,
    });
    expect(obj.propertyIsEnumerable('visible')).toBe(true);
  });

  test('array indices are enumerable', () => {
    const arr = [10, 20, 30];
    expect(arr.propertyIsEnumerable('0')).toBe(true);
    expect(arr.propertyIsEnumerable('1')).toBe(true);
    expect(arr.propertyIsEnumerable('2')).toBe(true);
  });

  test('array length is not enumerable', () => {
    const arr = [1, 2, 3];
    expect(arr.propertyIsEnumerable('length')).toBe(false);
  });

  test('called with no arguments checks "undefined" key', () => {
    const obj = { undefined: 1 };
    expect(obj.propertyIsEnumerable()).toBe(true);

    const obj2 = { x: 1 };
    expect(obj2.propertyIsEnumerable()).toBe(false);
  });

  test('coerces argument to string', () => {
    const obj = { 1: 'one', true: 'yes' };
    expect(obj.propertyIsEnumerable(1)).toBe(true);
    expect(obj.propertyIsEnumerable(true)).toBe(true);
  });

  test('accepts property keys that coerce to symbols', () => {
    let calls = 0;
    const sym = Symbol('visible');
    const obj = { [sym]: 1 };
    const key = {
      [Symbol.toPrimitive]() {
        calls++;
        return sym;
      },
    };

    expect(obj.propertyIsEnumerable(key)).toBe(true);
    expect(calls).toBe(1);
  });

  test('boxes string receivers before reading own descriptors', () => {
    expect(Object.prototype.propertyIsEnumerable.call('hi', '0')).toBe(true);
    expect(Object.prototype.propertyIsEnumerable.call('hi', 'length')).toBe(false);
  });

  test('class instance own properties are enumerable', () => {
    class Foo {
      constructor() {
        this.x = 1;
      }
      method() { return 2; }
    }
    const f = new Foo();
    expect(f.propertyIsEnumerable('x')).toBe(true);
    expect(f.propertyIsEnumerable('method')).toBe(false);
  });
});
