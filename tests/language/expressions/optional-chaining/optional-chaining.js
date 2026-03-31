describe('Optional Chaining', () => {
  test('returns property value when object exists', () => {
    const obj = { a: { b: 42 } };
    expect(obj?.a?.b).toBe(42);
  });

  test('returns undefined when object is null', () => {
    const obj = null;
    expect(obj?.a).toBe(undefined);
  });

  test('returns undefined when object is undefined', () => {
    const obj = undefined;
    expect(obj?.a).toBe(undefined);
  });

  test('returns undefined for nested null access', () => {
    const obj = { a: null };
    expect(obj?.a?.b).toBe(undefined);
  });

  test('works with computed property access', () => {
    const obj = { a: { b: 42 } };
    const key = 'b';
    expect(obj?.a?.[key]).toBe(42);
  });

  test('returns undefined for computed access on null', () => {
    const obj = null;
    expect(obj?.['a']).toBe(undefined);
  });

  test('works with method calls on existing object', () => {
    const obj = { greet: () => 'hello' };
    expect(obj?.greet()).toBe('hello');
  });

  test('chains multiple levels safely', () => {
    const obj = { a: { b: { c: 'deep' } } };
    expect(obj?.a?.b?.c).toBe('deep');
    expect(obj?.x?.y?.z).toBe(undefined);
  });

  test('does not evaluate call arguments after an optional short-circuit', () => {
    const obj = null;
    let calls = 0;
    const sideEffect = () => {
      calls = calls + 1;
      return 42;
    };

    expect(obj?.method(sideEffect())).toBe(undefined);
    expect(calls).toBe(0);
  });

  test('evaluates the optional chain base only once', () => {
    let reads = 0;
    const holder = {
      get value() {
        reads = reads + 1;
        return { answer: 42 };
      },
    };

    expect(holder.value?.answer).toBe(42);
    expect(reads).toBe(1);
  });

  test('grouping ends the optional chain', () => {
    const obj = null;

    expect(() => (obj?.a).b).toThrow(TypeError);
  });

  test('does not evaluate computed keys after an optional short-circuit', () => {
    const obj = null;
    let reads = 0;
    const key = () => {
      reads = reads + 1;
      return 'value';
    };

    expect(obj?.[key()]).toBe(undefined);
    expect(reads).toBe(0);
  });

  test('preserves call-site this for optional method calls', () => {
    const obj = {
      value: 42,
      getValue() {
        return this.value;
      },
    };

    expect(obj?.getValue()).toBe(42);
  });
});
