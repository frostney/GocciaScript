describe("toContain", () => {
  test("finds an array element by identity", () => {
    expect([1, 2, 3]).toContain(2);
    expect([1, 2, 3]).not.toContain(4);
  });

  test("finds a Set member", () => {
    expect(new Set([1, 2])).toContain(1);
    expect(new Set([1, 2])).not.toContain(3);
  });

  test("finds a substring", () => {
    expect("GocciaScript").toContain("Script");
    expect("GocciaScript").not.toContain("Vitest");
  });

  test("distinguishes -0 from 0", () => {
    // Protected parity: goccia and vitest agree, bun does not.
    expect([0]).toContain(-0);
    expect([-0]).toContain(0);
  });
});
