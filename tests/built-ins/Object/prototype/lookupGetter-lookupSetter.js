/*---
description: Object.prototype.__lookupGetter__ and __lookupSetter__
features: [Object.prototype.__lookupGetter__, Object.prototype.__lookupSetter__]
---*/

const hasLookupAccessors =
  typeof Object !== 'undefined' &&
  typeof Object.prototype.__lookupGetter__ === 'function' &&
  typeof Object.prototype.__lookupSetter__ === 'function';

describe.runIf(hasLookupAccessors)('Object.prototype.__lookupGetter__ and __lookupSetter__', () => {
  test('finds own string accessors', () => {
    const obj = {};
    const getter = () => 1;
    const setter = (value) => value;
    Object.defineProperty(obj, 'value', {
      get: getter,
      set: setter,
      configurable: true
    });

    expect(obj.__lookupGetter__('value')).toBe(getter);
    expect(obj.__lookupSetter__('value')).toBe(setter);
  });

  test('finds inherited accessors', () => {
    const parent = {};
    const child = Object.create(parent);
    const getter = () => 2;
    const setter = (value) => value;
    Object.defineProperty(parent, 'value', {
      get: getter,
      set: setter,
      configurable: true
    });

    expect(child.__lookupGetter__('value')).toBe(getter);
    expect(child.__lookupSetter__('value')).toBe(setter);
  });

  test('preserves symbol property keys', () => {
    const key = Symbol('value');
    const obj = {};
    const getter = () => 3;
    const setter = (value) => value;
    Object.defineProperty(obj, key, {
      get: getter,
      set: setter,
      configurable: true
    });

    expect(obj.__lookupGetter__(key)).toBe(getter);
    expect(obj.__lookupSetter__(key)).toBe(setter);
  });

  test('coerces property keys once before walking prototypes', () => {
    let getterCoercions = 0;
    let setterCoercions = 0;
    const getterKey = {
      toString() {
        getterCoercions++;
        return 'getterValue';
      }
    };
    const setterKey = {
      toString() {
        setterCoercions++;
        return 'setterValue';
      }
    };
    const parent = {};
    const child = Object.create(parent);
    const getter = () => 5;
    const setter = (value) => value;

    Object.defineProperty(parent, 'getterValue', {
      get: getter,
      configurable: true
    });
    Object.defineProperty(parent, 'setterValue', {
      set: setter,
      configurable: true
    });

    expect(child.__lookupGetter__(getterKey)).toBe(getter);
    expect(child.__lookupSetter__(setterKey)).toBe(setter);
    expect(getterCoercions).toBe(1);
    expect(setterCoercions).toBe(1);
  });

  test('returns undefined for data descriptors and missing properties', () => {
    const obj = { value: 4 };

    expect(obj.__lookupGetter__('value')).toBe(undefined);
    expect(obj.__lookupSetter__('value')).toBe(undefined);
    expect(obj.__lookupGetter__('missing')).toBe(undefined);
    expect(obj.__lookupSetter__('missing')).toBe(undefined);
  });
});
