describe("Atomics.isLockFree", () => {
  test("reports lock-free element sizes", () => {
    expect(Atomics.isLockFree(1)).toBe(true);
    expect(Atomics.isLockFree(2)).toBe(true);
    expect(Atomics.isLockFree(4)).toBe(true);
    expect(Atomics.isLockFree(8)).toBe(true);
  });

  test("reports non-element sizes as not lock-free", () => {
    expect(Atomics.isLockFree(3)).toBe(false);
    expect(Atomics.isLockFree(16)).toBe(false);
  });
});
