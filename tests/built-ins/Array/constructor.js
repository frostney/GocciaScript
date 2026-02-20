describe("Array constructor", () => {
  test("new Array() creates empty array", () => {
    const arr = new Array();
    expect(arr.length).toBe(0);
    expect(Array.isArray(arr)).toBe(true);
  });

  test("new Array(n) creates array with given length", () => {
    const arr = new Array(3);
    expect(arr.length).toBe(3);
  });

  test("new Array(1, 2, 3) creates array with elements", () => {
    const arr = new Array(1, 2, 3);
    expect(arr.length).toBe(3);
    expect(arr[0]).toBe(1);
    expect(arr[1]).toBe(2);
    expect(arr[2]).toBe(3);
  });

  test("new Array() has array methods", () => {
    const arr = new Array(1, 2, 3);
    const mapped = arr.map((x) => x * 2);
    expect(mapped[0]).toBe(2);
    expect(mapped[1]).toBe(4);
    expect(mapped[2]).toBe(6);
  });

  test("new Array() instanceof Array", () => {
    const arr = new Array();
    expect(arr instanceof Array).toBe(true);
  });
});
