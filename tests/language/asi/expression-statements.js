/*---
description: ASI for expression statements
features: [automatic-semicolon-insertion]
---*/

describe("ASI expression statements", () => {
  test("function call without semicolon", () => {
    let x = 0;
    const inc = () => { x = x + 1 }
    inc()
    inc()
    expect(x).toBe(2);
  });

  test("assignment without semicolon", () => {
    let x = 0
    x = 10
    expect(x).toBe(10);
  });

  test("method call without semicolon", () => {
    const arr = [3, 1, 2]
    const sorted = arr.slice().sort((a, b) => a - b)
    expect(sorted[0]).toBe(1)
    expect(sorted[2]).toBe(3)
  });

  test("expression before closing brace", () => {
    let x = 0;
    const fn = () => { x = 42 }
    fn();
    expect(x).toBe(42);
  });

  // ES2026 §13.4 restricted production: no LineTerminator between a
  // LeftHandSideExpression and ++/--.  A newline before ++/-- rejects the
  // postfix parse and ASI inserts a semicolon, so ++/-- binds as prefix
  // to the following line.
  test("newline before ++ inserts ASI and parses ++ as prefix", () => {
    let x = 0
    let y = 0
    x
    ++y
    expect(x).toBe(0);
    expect(y).toBe(1);
  });

  test("newline before -- inserts ASI and parses -- as prefix", () => {
    let x = 1
    let y = 1
    x
    --y
    expect(x).toBe(1);
    expect(y).toBe(0);
  });

  test("assignment followed by newline then ++ id does not attach to assignment", () => {
    let a = 1, b = 2, c = 3
    a = b
    ++c
    expect(a).toBe(b);
    expect(c).toBe(4);
  });

  test("id on one line, ++ on next, ident on third — ASI + prefix", () => {
    let x = 0, y = 0
    x
    ++
    y
    expect(x).toBe(0);
    expect(y).toBe(1);
  });

  test("id on one line, -- on next, ident on third — ASI + prefix", () => {
    let x = 0, y = 2
    x
    --
    y
    expect(x).toBe(0);
    expect(y).toBe(1);
  });

  test("postfix ++ still works when on the same line", () => {
    let x = 5
    const result = x++
    expect(result).toBe(5);
    expect(x).toBe(6);
  });

  test("postfix -- still works when on the same line", () => {
    let x = 5
    const result = x--
    expect(result).toBe(5);
    expect(x).toBe(4);
  });
});
