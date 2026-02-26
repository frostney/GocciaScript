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
});
