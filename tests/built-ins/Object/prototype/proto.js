/*---
description: Object.prototype.__proto__
features: [Object.prototype.__proto__]
---*/

const hasProtoAccessor =
  typeof Object !== 'undefined' &&
  Object.getOwnPropertyDescriptor(Object.prototype, '__proto__') !== undefined;

describe.runIf(hasProtoAccessor)('Object.prototype.__proto__', () => {
  test('is a configurable non-enumerable accessor', () => {
    const descriptor = Object.getOwnPropertyDescriptor(Object.prototype, '__proto__');

    expect(typeof descriptor.get).toBe('function');
    expect(typeof descriptor.set).toBe('function');
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(true);
  });

  test('getter returns the receiver prototype', () => {
    const proto = { marker: true };
    const obj = Object.create(proto);

    expect(obj.__proto__).toBe(proto);
  });

  test('setter changes the receiver prototype', () => {
    const proto = { marker: true };
    const obj = {};

    obj.__proto__ = proto;

    expect(Object.getPrototypeOf(obj)).toBe(proto);
    expect(obj.marker).toBe(true);
  });

  test('setter accepts null', () => {
    const obj = {};

    obj.__proto__ = null;

    expect(Object.getPrototypeOf(obj)).toBe(null);
  });

  test('setter ignores non-object prototype values', () => {
    const obj = {};
    const original = Object.getPrototypeOf(obj);

    obj.__proto__ = 1;
    expect(Object.getPrototypeOf(obj)).toBe(original);

    obj.__proto__ = 'not a prototype';
    expect(Object.getPrototypeOf(obj)).toBe(original);
  });

  test('accessor rejects nullish receivers', () => {
    const descriptor = Object.getOwnPropertyDescriptor(Object.prototype, '__proto__');

    expect(() => descriptor.get.call(null)).toThrow(TypeError);
    expect(() => descriptor.get.call(undefined)).toThrow(TypeError);
    expect(() => descriptor.set.call(null, {})).toThrow(TypeError);
    expect(() => descriptor.set.call(undefined, {})).toThrow(TypeError);
  });
});
