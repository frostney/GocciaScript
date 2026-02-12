describe('Rest Parameters', () => {
  test('collects remaining arguments into an array', () => {
    const fn = (...args) => args.length;
    expect(fn(1, 2, 3)).toBe(3);
  });

  test('rest parameter is an array', () => {
    const fn = (...args) => args[0] + args[1];
    expect(fn(10, 20)).toBe(30);
  });

  test('rest after regular parameters', () => {
    const fn = (first, ...rest) => rest.length;
    expect(fn(1, 2, 3, 4)).toBe(3);
  });

  test('rest parameter is empty array when no extra args', () => {
    const fn = (a, ...rest) => rest.length;
    expect(fn(1)).toBe(0);
  });

  test('rest with no arguments produces empty array', () => {
    const fn = (...args) => args.length;
    expect(fn()).toBe(0);
  });

  test('rest parameter works in object methods', () => {
    const obj = {
      sum(...nums) {
        return nums.reduce((acc, n) => acc + n, 0);
      }
    };
    expect(obj.sum(1, 2, 3)).toBe(6);
  });
});
