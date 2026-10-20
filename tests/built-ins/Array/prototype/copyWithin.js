describe("Array.prototype.copyWithin", () => {
  test("copies elements to target position", () => {
    expect([1, 2, 3, 4, 5].copyWithin(0, 3)).toEqual([4, 5, 3, 4, 5]);
  });

  test("copies with start and end", () => {
    expect([1, 2, 3, 4, 5].copyWithin(1, 3, 4)).toEqual([1, 4, 3, 4, 5]);
  });

  test("negative target counts from end", () => {
    expect([1, 2, 3, 4, 5].copyWithin(-2, 0, 2)).toEqual([1, 2, 3, 1, 2]);
  });

  test("returns the modified array", () => {
    const arr = [1, 2, 3];
    const result = arr.copyWithin(0, 1);
    expect(result).toBe(arr);
  });

  test("overlapping copy with target > start", () => {
    expect([1, 2, 3, 4, 5].copyWithin(2, 0, 3)).toEqual([1, 2, 1, 2, 3]);
  });

  test("no effect when count is zero", () => {
    expect([1, 2, 3].copyWithin(0, 1, 1)).toEqual([1, 2, 3]);
  });

  test("generic receiver copies within array-like", () => {
    const obj = { 0: 'a', 1: 'b', 2: 'c', 3: 'd', length: 4 };
    Array.prototype.copyWithin.call(obj, 0, 2, 4);
    expect(obj[0]).toBe('c');
    expect(obj[1]).toBe('d');
  });

  test("copyWithin preserves holes in sparse array", () => {
    const arr = [1, , 3, 4];
    arr.copyWithin(0, 1, 3);
    expect(0 in arr).toBe(false);
    expect(arr[1]).toBe(3);
  });

  test("reads and writes one element at a time, in the specified order", () => {
    const log = [];
    const backing = { 0: "a", 1: "b", 2: "c", 3: "d" };
    const target = new Proxy({ ...backing, length: 4 }, {
      get(t, k, r) {
        if (k !== "length") log.push(`get ${String(k)}`);
        return Reflect.get(t, k, r);
      },
      set(t, k, v, r) {
        log.push(`set ${String(k)}=${v}`);
        return Reflect.set(t, k, v, r);
      },
    });
    // The source starts before an overlapping target, so it copies backwards.
    Array.prototype.copyWithin.call(target, 1, 0, 3);
    expect(log).toEqual(["get 2", "set 3=c", "get 1", "set 2=b", "get 0", "set 1=a"]);
    log.length = 0;
    Array.prototype.copyWithin.call(target, 0, 2, 4);
    expect(log).toEqual(["get 2", "set 0=b", "get 3", "set 1=c"]);
  });
});
