/*---
description: Ternary operator precedence - consequent uses AssignmentExpression
features: [conditional, ternary]
---*/

describe("ternary operator precedence", () => {
  test("basic ternary", () => {
    expect(true ? 1 : 2).toBe(1);
    expect(false ? 1 : 2).toBe(2);
  });

  test("nested ternary in alternate", () => {
    const x = true ? 1 : false ? 2 : 3;
    expect(x).toBe(1);

    const y = false ? 1 : true ? 2 : 3;
    expect(y).toBe(2);

    const z = false ? 1 : false ? 2 : 3;
    expect(z).toBe(3);
  });

  test("ternary with assignment expressions", () => {
    let a = 0;
    const result = true ? a = 5 : a = 10;
    expect(result).toBe(5);
    expect(a).toBe(5);
  });

  test("ternary does not evaluate unused branch", () => {
    let sideEffect = 0;
    const inc = () => { sideEffect = 1; return 99; };
    const result = true ? 42 : inc();
    expect(result).toBe(42);
    expect(sideEffect).toBe(0);
  });
});
