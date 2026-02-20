describe('Array.prototype.find', () => {
  test('returns the first element that satisfies the predicate', () => {
    const arr = [1, 2, 3, 4, 5];
    expect(arr.find((x) => x > 3)).toBe(4);
  });

  test('returns undefined if no element satisfies the predicate', () => {
    const arr = [1, 2, 3];
    expect(arr.find((x) => x > 10)).toBe(undefined);
  });

  test('passes element, index, and array to callback', () => {
    const arr = [10, 20, 30];
    let foundIndex = -1;
    arr.find((elem, idx) => {
      if (elem === 20) {
        foundIndex = idx;
        return true;
      }
      return false;
    });
    expect(foundIndex).toBe(1);
  });

  test('works with objects in array', () => {
    const arr = [{ name: 'a' }, { name: 'b' }, { name: 'c' }];
    const result = arr.find((item) => item.name === 'b');
    expect(result.name).toBe('b');
  });

  test('returns the same object reference, not a copy', () => {
    const obj = { x: 1 };
    const arr = [obj];
    expect(arr.find((item) => item.x === 1)).toBe(obj);
  });

  test('callback receives element, index, and array', () => {
    const arr = [10, 20, 30];
    const collected = [];
    arr.find((val, idx, a) => {
      collected.push({ val, idx, len: a.length });
      return false;
    });
    expect(collected.length).toBe(3);
    expect(collected[0].val).toBe(10);
    expect(collected[0].idx).toBe(0);
    expect(collected[0].len).toBe(3);
  });

  test('stops iteration after first match', () => {
    let callCount = 0;
    [1, 2, 3, 4, 5].find((x) => {
      callCount = callCount + 1;
      return x === 3;
    });
    expect(callCount).toBe(3);
  });

  test('find on empty array returns undefined', () => {
    expect([].find(() => true)).toBe(undefined);
  });

  test('finds undefined in array', () => {
    expect([1, undefined, 3].find((x) => x === undefined)).toBe(undefined);
  });

  test('finds null in array', () => {
    expect([1, null, 3].find((x) => x === null)).toBe(null);
  });

  test('finds NaN in array', () => {
    const result = [1, NaN, 3].find((x) => Number.isNaN(x));
    expect(Number.isNaN(result)).toBe(true);
  });
});
