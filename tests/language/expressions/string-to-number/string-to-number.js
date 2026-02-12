describe('String to Number Conversion', () => {
  test('empty string converts to 0', () => {
    expect(+"").toBe(0);
  });

  test('whitespace-only string converts to 0', () => {
    expect(+"  ").toBe(0);
  });

  test('leading/trailing whitespace is trimmed', () => {
    expect(+"  42  ").toBe(42);
  });

  test('Infinity string converts to Infinity', () => {
    expect(+"Infinity").toBe(Infinity);
  });

  test('-Infinity string converts to -Infinity', () => {
    expect(+"-Infinity").toBe(-Infinity);
  });

  test('hex string converts to number', () => {
    expect(+"0xFF").toBe(255);
    expect(+"0x10").toBe(16);
  });

  test('non-numeric string converts to NaN', () => {
    expect(Number.isNaN(+"abc")).toBe(true);
  });

  test('empty array converts to 0', () => {
    const arr = [];
    expect(+arr).toBe(0);
  });

  test('single element array converts to that element', () => {
    const arr = [42];
    expect(+arr).toBe(42);
  });

  test('type coercion in comparison uses ToNumber', () => {
    expect("10" > 2).toBe(true);
    expect(true > 0).toBe(true);
  });
});
