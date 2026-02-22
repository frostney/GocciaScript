/*---
description: >
  Unsupported features (for, while, do...while, var, with, function, ==, !=,
  export *, labels) are parsed as no-ops. Code after skipped statements must
  still execute correctly, even when the unsupported statement contains nested
  parentheses or complex expressions.
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

  test("function declaration is skipped", () => {
    let x = 1;
    function foo() { x = 99; }
    expect(x).toBe(1);
    expect(typeof foo).toBe("undefined");
  });

  test("named function declaration with parameters is skipped", () => {
    let x = 1;
    function add(a, b) { return a + b; }
    expect(x).toBe(1);
    expect(typeof add).toBe("undefined");
  });

  test("function expression evaluates to undefined", () => {
    const fn = function() { return 42; };
    expect(fn).toBe(undefined);
  });

  test("named function expression evaluates to undefined", () => {
    const fn = function myFunc(a, b) { return a + b; };
    expect(fn).toBe(undefined);
  });

  test("generator function declaration is skipped", () => {
    let x = 1;
    function* gen() { x = 99; }
    expect(x).toBe(1);
    expect(typeof gen).toBe("undefined");
  });

  test("code after function declaration continues correctly", () => {
    let x = 1;
    function ignored() { x = 99; }
    x = 2;
    expect(x).toBe(2);
  });

  test("== (loose equality) evaluates to undefined", () => {
    const a = 1;
    const b = 1;
    const result = a == b;
    expect(result).toBe(undefined);
  });

  test("!= (loose inequality) evaluates to undefined", () => {
    const a = 1;
    const b = 2;
    const result = a != b;
    expect(result).toBe(undefined);
  });

  test("== in if condition is falsy", () => {
    let x = 1;
    if (1 == 1) { x = 99; }
    expect(x).toBe(1);
  });

  test("!= in if condition is falsy", () => {
    let x = 1;
    if (1 != 2) { x = 99; }
    expect(x).toBe(1);
  });

  test("multiple unsupported features together", () => {
    let x = 1;
    function ignored() { x = 99; }
    for (let i = 0; i < 10; i++) { x = 99; }
    var z = 42;
    x = 2;
    expect(x).toBe(2);
  });

  test("labeled statement skips the label but executes the statement", () => {
    let x = 1;
    myLabel: x = 2;
    expect(x).toBe(2);
  });

  test("label on a skipped for loop", () => {
    let x = 1;
    outer: for (let i = 0; i < 10; i++) { x = 99; }
    expect(x).toBe(1);
  });

  test("label on a block statement", () => {
    let x = 1;
    myLabel: {
      x = 2;
    }
    expect(x).toBe(2);
  });

  test("code after labeled statement continues correctly", () => {
    let x = 1;
    myLabel: x = 2;
    const y = x + 1;
    expect(y).toBe(3);
  });
});
