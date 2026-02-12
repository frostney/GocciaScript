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
});
