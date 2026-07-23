// Run: ./build/GocciaTestRunner examples/_experimental/testrunner-vitest.js
// Update snapshots: add --update (the Vitest-compatible alias)
const cart = [];

describe("checkout", () => {
  beforeEach(() => { cart.length = 0; });

  test.each([["coffee", 3], ["cake", 4]])("%s costs £%d", (item, price) => {
    cart.push({ item, price });
    expect(cart).toContainEqual({ item, price });
  });

  test("receipt formatting matches Vitest", () => {
    expect({ currency: "GBP", total: 7 }).toMatchInlineSnapshot(`
{
  "currency": "GBP",
  "total": 7,
}
`);
  });
});
