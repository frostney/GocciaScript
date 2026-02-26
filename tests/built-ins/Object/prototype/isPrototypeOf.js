describe('Object.prototype.isPrototypeOf', () => {
  test('basic prototype chain check', () => {
    const parent = { x: 1 };
    const child = Object.create(parent);
    expect(parent.isPrototypeOf(child)).toBe(true);
  });

  test('returns false when not in chain', () => {
    const a = { x: 1 };
    const b = { y: 2 };
    expect(a.isPrototypeOf(b)).toBe(false);
  });

  test('works with multi-level prototype chains', () => {
    const grandparent = {};
    const parent = Object.create(grandparent);
    const child = Object.create(parent);

    expect(grandparent.isPrototypeOf(child)).toBe(true);
    expect(parent.isPrototypeOf(child)).toBe(true);
    expect(child.isPrototypeOf(grandparent)).toBe(false);
  });

  test('works with class inheritance', () => {
    class Animal {}
    class Dog extends Animal {}
    class Puppy extends Dog {}

    const p = new Puppy();
    expect(Animal.prototype.isPrototypeOf(p)).toBe(true);
    expect(Dog.prototype.isPrototypeOf(p)).toBe(true);
    expect(Puppy.prototype.isPrototypeOf(p)).toBe(true);
  });

  test('returns false for non-object argument', () => {
    const obj = {};
    expect(obj.isPrototypeOf(42)).toBe(false);
    expect(obj.isPrototypeOf('string')).toBe(false);
    expect(obj.isPrototypeOf(true)).toBe(false);
    expect(obj.isPrototypeOf(null)).toBe(false);
    expect(obj.isPrototypeOf(undefined)).toBe(false);
  });

  test('returns false when called with no arguments', () => {
    const obj = {};
    expect(obj.isPrototypeOf()).toBe(false);
  });

  test('an object is not its own prototype', () => {
    const obj = {};
    expect(obj.isPrototypeOf(obj)).toBe(false);
  });

  test('Object.prototype is in chain of most objects', () => {
    const obj = {};
    const arr = [1, 2, 3];

    expect(Object.prototype.isPrototypeOf(obj)).toBe(true);
    expect(Object.prototype.isPrototypeOf(arr)).toBe(true);
  });

  test('works with Object.create(null)', () => {
    const nullProto = Object.create(null);
    const obj = {};
    expect(obj.isPrototypeOf(nullProto)).toBe(false);
  });

  test('throws TypeError when called on null or undefined', () => {
    const fn = Object.prototype.isPrototypeOf;
    expect(() => fn.call(null, {})).toThrow(TypeError);
    expect(() => fn.call(undefined, {})).toThrow(TypeError);
  });
});
