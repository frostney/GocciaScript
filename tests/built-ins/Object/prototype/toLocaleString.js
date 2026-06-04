describe('Object.prototype.toLocaleString', () => {
  test('calls toString() by default', () => {
    const obj = {};
    expect(obj.toLocaleString()).toBe(obj.toString());
  });

  test('uses custom toString when defined', () => {
    const obj = {
      toString() { return 'custom'; },
    };
    expect(obj.toLocaleString()).toBe('custom');
  });

  test('calls proxied toString methods', () => {
    const obj = {
      toString: new Proxy(() => 'proxied', {}),
    };

    expect(obj.toLocaleString()).toBe('proxied');
  });

  test('works on arrays', () => {
    const arr = [1, 2, 3];
    expect(arr.toLocaleString()).toBe(arr.toString());
  });

  test('works on class instances', () => {
    class Foo {
      toString() { return 'Foo instance'; }
    }
    const f = new Foo();
    expect(f.toLocaleString()).toBe('Foo instance');
  });

  test('returns [object Object] for plain objects', () => {
    const obj = {};
    expect(obj.toLocaleString()).toBe('[object Object]');
  });

  test('follows toString override in prototype chain', () => {
    class Base {
      toString() { return 'Base'; }
    }
    class Child extends Base {}
    const c = new Child();
    expect(c.toLocaleString()).toBe('Base');
  });

  test('rejects nullish receivers', () => {
    const toLocaleString = Object.prototype.toLocaleString;

    expect(() => toLocaleString.call(null)).toThrow(TypeError);
    expect(() => toLocaleString.call(undefined)).toThrow(TypeError);
    expect(() => toLocaleString()).toThrow(TypeError);
  });

  test('throws when toString is not callable', () => {
    expect(() => Object.prototype.toLocaleString.call({ toString: 1 })).toThrow(TypeError);
  });

  test('uses primitive receiver toString methods', () => {
    expect(Object.prototype.toLocaleString.call(1)).toBe('1');
    expect(Object.prototype.toLocaleString.call(true)).toBe('true');
    expect(Object.prototype.toLocaleString.call('x')).toBe('x');
  });
});
