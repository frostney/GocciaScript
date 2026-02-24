describe('Object.prototype.valueOf', () => {
  test('returns the object itself', () => {
    const obj = { x: 1 };
    expect(obj.valueOf()).toBe(obj);
  });

  test('identity for plain objects', () => {
    const a = {};
    const b = {};
    expect(a.valueOf()).toBe(a);
    expect(a.valueOf()).not.toBe(b);
  });

  test('works on arrays', () => {
    const arr = [1, 2, 3];
    expect(arr.valueOf()).toBe(arr);
  });

  test('works on class instances', () => {
    class Foo {
      constructor() { this.x = 42; }
    }
    const f = new Foo();
    expect(f.valueOf()).toBe(f);
    expect(f.valueOf().x).toBe(42);
  });

  test('can be overridden', () => {
    const obj = {
      valueOf() { return 42; },
    };
    expect(obj.valueOf()).toBe(42);
  });

  test('override in class', () => {
    class MyNum {
      constructor() { this.value = 10; }
      valueOf() { return this.value; }
    }
    const n = new MyNum();
    expect(n.valueOf()).toBe(10);
  });

  test('used by numeric coercion via ToPrimitive', () => {
    const obj = {
      valueOf() { return 5; },
    };
    expect(obj + 3).toBe(8);
  });

  test('inherited valueOf returns the object', () => {
    const parent = {};
    const child = Object.create(parent);
    expect(child.valueOf()).toBe(child);
  });
});
