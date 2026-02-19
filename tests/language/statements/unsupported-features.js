/*---
description: >
  Unsupported features (for, while, do...while, var, with) are parsed as no-ops.
  Code after skipped statements must still execute correctly, even when the
  unsupported statement contains nested parentheses or complex expressions.
features: [parser-warnings, unsupported-features]
---*/

const isGocciaScript = typeof GocciaScript !== "undefined";

describe.runIf(isGocciaScript)("unsupported features are skipped", () => {
  test("code after skipped for loop executes", () => {
    let x = 1;
    for (let i = 0; i < 10; i++) { x = 99; }
    expect(x).toBe(1);
  });

  test("for loop with nested parentheses is skipped correctly", () => {
    let x = 1;
    for (let i = Math.max(0, 1); i < Math.min(10, 20); i++) { x = 99; }
    expect(x).toBe(1);
  });

  test("for loop with function call in condition is skipped", () => {
    let x = 1;
    const arr = [1, 2, 3];
    for (let i = 0; i < arr.indexOf(2); i++) { x = 99; }
    expect(x).toBe(1);
  });

  test("code after skipped while loop executes", () => {
    let x = 1;
    while (x < 10) { x = 99; }
    expect(x).toBe(1);
  });

  test("while loop with nested parentheses is skipped correctly", () => {
    let x = 1;
    while (Math.max(x, 0) < 10) { x = 99; }
    expect(x).toBe(1);
  });

  test("code after skipped do...while executes", () => {
    let x = 1;
    do { x = 99; } while (x < 10);
    expect(x).toBe(1);
  });

  test("do...while with nested parentheses is skipped correctly", () => {
    let x = 1;
    do { x = 99; } while (Math.max(x, 0) < 10);
    expect(x).toBe(1);
  });

  test("code after skipped var declaration executes", () => {
    let x = 1;
    var y = 42;
    expect(x).toBe(1);
    expect(typeof y).toBe("undefined");
  });

  test("multiple unsupported statements in sequence", () => {
    let x = 1;
    for (let i = 0; i < 10; i++) { x = 99; }
    while (x < 10) { x = 99; }
    do { x = 99; } while (x < 10);
    var z = 42;
    expect(x).toBe(1);
  });
});
