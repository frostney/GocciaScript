/*---
description: Object.prototype.__defineGetter__ and __defineSetter__
features: [Object.prototype.__defineGetter__, Object.prototype.__defineSetter__]
---*/

const hasDefineAccessors =
  typeof Object !== 'undefined' &&
  typeof Object.prototype.__defineGetter__ === 'function' &&
  typeof Object.prototype.__defineSetter__ === 'function';

describe.runIf(hasDefineAccessors)('Object.prototype.__defineGetter__ and __defineSetter__', () => {
  test('defines enumerable configurable accessors', () => {
    const obj = {};
    const getter = () => 1;
    const setter = (value) => value;

    expect(obj.__defineGetter__('value', getter)).toBe(undefined);
    expect(obj.__defineSetter__('value', setter)).toBe(undefined);

    const descriptor = Object.getOwnPropertyDescriptor(obj, 'value');
    expect(descriptor.get).toBe(getter);
    expect(descriptor.set).toBe(setter);
    expect(descriptor.enumerable).toBe(true);
    expect(descriptor.configurable).toBe(true);
  });

  test('preserves symbol property keys', () => {
    const key = Symbol('value');
    const obj = {};
    const getter = () => 2;
    const setter = (value) => value;

    obj.__defineGetter__(key, getter);
    obj.__defineSetter__(key, setter);

    const descriptor = Object.getOwnPropertyDescriptor(obj, key);
    expect(descriptor.get).toBe(getter);
    expect(descriptor.set).toBe(setter);
  });

  test('rejects non-callable accessors before coercing the key', () => {
    const obj = {};
    const key = {
      toString() {
        throw new Error('key should not be coerced');
      }
    };

    expect(() => obj.__defineGetter__(key, 1)).toThrow(TypeError);
    expect(() => obj.__defineSetter__(key, 1)).toThrow(TypeError);
  });

  test('has standard function metadata', () => {
    expect(Object.prototype.__defineGetter__.name).toBe('__defineGetter__');
    expect(Object.prototype.__defineGetter__.length).toBe(2);
    expect(Object.prototype.__defineSetter__.name).toBe('__defineSetter__');
    expect(Object.prototype.__defineSetter__.length).toBe(2);
  });
});
