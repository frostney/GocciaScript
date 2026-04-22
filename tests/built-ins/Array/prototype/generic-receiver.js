// ES2026 §23.1.3: Array.prototype methods accept any object with a
// `length` property as `this` via ToObject coercion.

describe('Array.prototype generic receiver (array-like objects)', () => {
  const arrayLike = { 0: 'a', 1: 'b', 2: 'c', length: 3 };

  test('indexOf finds element in array-like', () => {
    expect(Array.prototype.indexOf.call(arrayLike, 'b')).toBe(1);
    expect(Array.prototype.indexOf.call(arrayLike, 'z')).toBe(-1);
  });

  test('lastIndexOf finds element in array-like', () => {
    const obj = { 0: 'x', 1: 'y', 2: 'x', length: 3 };
    expect(Array.prototype.lastIndexOf.call(obj, 'x')).toBe(2);
    expect(Array.prototype.lastIndexOf.call(obj, 'z')).toBe(-1);
  });

  test('includes searches array-like', () => {
    expect(Array.prototype.includes.call(arrayLike, 'a')).toBe(true);
    expect(Array.prototype.includes.call(arrayLike, 'z')).toBe(false);
  });

  test('join joins array-like elements', () => {
    expect(Array.prototype.join.call(arrayLike, '-')).toBe('a-b-c');
    expect(Array.prototype.join.call(arrayLike)).toBe('a,b,c');
  });

  test('slice extracts from array-like', () => {
    const result = Array.prototype.slice.call(arrayLike, 1, 3);
    expect(result).toEqual(['b', 'c']);
    expect(Array.isArray(result)).toBe(true);
  });

  test('concat does not spread non-array receiver (IsConcatSpreadable)', () => {
    // Plain objects are not concat-spreadable by default per ES spec
    const result = Array.prototype.concat.call(arrayLike, ['d']);
    expect(result.length).toBe(2);
    expect(result[0]).toBe(arrayLike);
    expect(result[1]).toBe('d');
  });

  test('concat spreads receiver with Symbol.isConcatSpreadable', () => {
    const obj = { 0: 'a', 1: 'b', length: 2, [Symbol.isConcatSpreadable]: true };
    const result = Array.prototype.concat.call(obj, ['c']);
    expect(result).toEqual(['a', 'b', 'c']);
  });

  test('find works on array-like', () => {
    const result = Array.prototype.find.call(arrayLike, x => x === 'b');
    expect(result).toBe('b');
  });

  test('findIndex works on array-like', () => {
    expect(Array.prototype.findIndex.call(arrayLike, x => x === 'b')).toBe(1);
    expect(Array.prototype.findIndex.call(arrayLike, x => x === 'z')).toBe(-1);
  });

  test('findLast works on array-like', () => {
    const obj = { 0: 'x', 1: 'y', 2: 'x', length: 3 };
    expect(Array.prototype.findLast.call(obj, x => x === 'x')).toBe('x');
  });

  test('findLastIndex works on array-like', () => {
    const obj = { 0: 'x', 1: 'y', 2: 'x', length: 3 };
    expect(Array.prototype.findLastIndex.call(obj, x => x === 'x')).toBe(2);
  });

  test('every checks all elements in array-like', () => {
    expect(Array.prototype.every.call(arrayLike, x => typeof x === 'string')).toBe(true);
    expect(Array.prototype.every.call(arrayLike, x => x === 'a')).toBe(false);
  });

  test('some checks elements in array-like', () => {
    expect(Array.prototype.some.call(arrayLike, x => x === 'b')).toBe(true);
    expect(Array.prototype.some.call(arrayLike, x => x === 'z')).toBe(false);
  });

  test('forEach iterates array-like', () => {
    const collected = [];
    Array.prototype.forEach.call(arrayLike, x => collected.push(x));
    expect(collected).toEqual(['a', 'b', 'c']);
  });

  test('map transforms array-like', () => {
    const result = Array.prototype.map.call(arrayLike, x => x.toUpperCase());
    expect(result).toEqual(['A', 'B', 'C']);
  });

  test('filter filters array-like', () => {
    const result = Array.prototype.filter.call(arrayLike, x => x !== 'b');
    expect(result).toEqual(['a', 'c']);
  });

  test('reduce accumulates over array-like', () => {
    const result = Array.prototype.reduce.call(arrayLike, (acc, x) => acc + x, '');
    expect(result).toBe('abc');
  });

  test('at accesses array-like by index', () => {
    expect(Array.prototype.at.call(arrayLike, 0)).toBe('a');
    expect(Array.prototype.at.call(arrayLike, -1)).toBe('c');
  });

  test('toReversed creates reversed copy from array-like', () => {
    const result = Array.prototype.toReversed.call(arrayLike);
    expect(result).toEqual(['c', 'b', 'a']);
  });

  test('toSorted creates sorted copy from array-like', () => {
    const obj = { 0: 'c', 1: 'a', 2: 'b', length: 3 };
    const result = Array.prototype.toSorted.call(obj);
    expect(result).toEqual(['a', 'b', 'c']);
  });

  test('toSpliced creates spliced copy from array-like', () => {
    const result = Array.prototype.toSpliced.call(arrayLike, 1, 1, 'x');
    expect(result).toEqual(['a', 'x', 'c']);
  });

  test('with creates copy with replaced index from array-like', () => {
    const result = Array.prototype.with.call(arrayLike, 1, 'x');
    expect(result).toEqual(['a', 'x', 'c']);
  });

  test('flat flattens array-like (elements that are arrays)', () => {
    const obj = { 0: [1, 2], 1: [3], length: 2 };
    const result = Array.prototype.flat.call(obj);
    expect(result).toEqual([1, 2, 3]);
  });

  test('flatMap maps and flattens array-like', () => {
    const nums = { 0: 1, 1: 2, 2: 3, length: 3 };
    const result = Array.prototype.flatMap.call(nums, x => [x, x * 2]);
    expect(result).toEqual([1, 2, 2, 4, 3, 6]);
  });
});

describe('Array.prototype generic receiver (primitive this via ToObject)', () => {
  test('indexOf on boolean returns -1', () => {
    expect(Array.prototype.indexOf.call(true, 1)).toBe(-1);
  });

  test('indexOf on number returns -1', () => {
    expect(Array.prototype.indexOf.call(42, 4)).toBe(-1);
  });

  test('includes on boolean returns false', () => {
    expect(Array.prototype.includes.call(false, false)).toBe(false);
  });

  test('join on empty-length primitive returns empty string', () => {
    expect(Array.prototype.join.call(true, ',')).toBe('');
  });

  test('slice on primitive returns empty array', () => {
    expect(Array.prototype.slice.call(42)).toEqual([]);
  });

  test('map on primitive returns empty array', () => {
    expect(Array.prototype.map.call(true, x => x)).toEqual([]);
  });
});

describe('Array.prototype generic receiver (null/undefined throws TypeError)', () => {
  test('indexOf throws on null this', () => {
    expect(() => Array.prototype.indexOf.call(null, 1)).toThrow();
  });

  test('indexOf throws on undefined this', () => {
    expect(() => Array.prototype.indexOf.call(undefined, 1)).toThrow();
  });

  test('forEach throws on null this', () => {
    expect(() => Array.prototype.forEach.call(null, x => x)).toThrow();
  });
});

describe('Array.prototype mutating methods on array-like', () => {
  test('reverse reverses array-like in place', () => {
    const obj = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
    const result = Array.prototype.reverse.call(obj);
    expect(result[0]).toBe('c');
    expect(result[1]).toBe('b');
    expect(result[2]).toBe('a');
  });

  test('fill fills array-like in place', () => {
    const obj = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
    Array.prototype.fill.call(obj, 'x', 1, 3);
    expect(obj[1]).toBe('x');
    expect(obj[2]).toBe('x');
    expect(obj[0]).toBe('a');
  });

  test('copyWithin copies within array-like', () => {
    const obj = { 0: 'a', 1: 'b', 2: 'c', 3: 'd', length: 4 };
    Array.prototype.copyWithin.call(obj, 0, 2, 4);
    expect(obj[0]).toBe('c');
    expect(obj[1]).toBe('d');
  });

  test('push appends to array-like', () => {
    const obj = { 0: 'a', length: 1 };
    const newLen = Array.prototype.push.call(obj, 'b');
    expect(newLen).toBe(2);
    expect(obj[1]).toBe('b');
    expect(obj.length).toBe(2);
  });

  test('pop removes last from array-like', () => {
    const obj = { 0: 'a', 1: 'b', length: 2 };
    const removed = Array.prototype.pop.call(obj);
    expect(removed).toBe('b');
    expect(obj.length).toBe(1);
  });

  test('shift removes first from array-like', () => {
    const obj = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
    const removed = Array.prototype.shift.call(obj);
    expect(removed).toBe('a');
    expect(obj[0]).toBe('b');
    expect(obj[1]).toBe('c');
    expect(obj.length).toBe(2);
  });

  test('unshift inserts at front of array-like', () => {
    const obj = { 0: 'b', 1: 'c', length: 2 };
    const newLen = Array.prototype.unshift.call(obj, 'a');
    expect(newLen).toBe(3);
    expect(obj[0]).toBe('a');
    expect(obj[1]).toBe('b');
    expect(obj[2]).toBe('c');
    expect(obj.length).toBe(3);
  });

  test('splice removes and inserts in array-like', () => {
    const obj = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
    const removed = Array.prototype.splice.call(obj, 1, 1, 'x', 'y');
    expect(removed).toEqual(['b']);
    expect(obj[0]).toBe('a');
    expect(obj[1]).toBe('x');
    expect(obj[2]).toBe('y');
    expect(obj[3]).toBe('c');
    expect(obj.length).toBe(4);
  });
});

describe('Array.prototype iterator methods on array-like', () => {
  test('values iterates array-like', () => {
    const obj = { 0: 'a', 1: 'b', length: 2 };
    const iter = Array.prototype.values.call(obj);
    expect(iter.next().value).toBe('a');
    expect(iter.next().value).toBe('b');
    expect(iter.next().done).toBe(true);
  });

  test('keys iterates array-like indices', () => {
    const obj = { 0: 'a', 1: 'b', length: 2 };
    const iter = Array.prototype.keys.call(obj);
    expect(iter.next().value).toBe(0);
    expect(iter.next().value).toBe(1);
    expect(iter.next().done).toBe(true);
  });

  test('entries iterates array-like as [index, value] pairs', () => {
    const obj = { 0: 'a', 1: 'b', length: 2 };
    const iter = Array.prototype.entries.call(obj);
    const first = iter.next().value;
    expect(first[0]).toBe(0);
    expect(first[1]).toBe('a');
    const second = iter.next().value;
    expect(second[0]).toBe(1);
    expect(second[1]).toBe('b');
    expect(iter.next().done).toBe(true);
  });
});
