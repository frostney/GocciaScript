/*---
description: Loose equality operators after type assertions
features: [compat-loose-equality, types-as-comments]
---*/

describe("loose equality with type assertions", () => {
  test("type assertions stop before loose equality operators", () => {
    const one = 1;

    expect(one as number == "1").toBe(true);
    expect(one as number != "1").toBe(false);
  });
});
