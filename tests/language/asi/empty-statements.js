/*---
description: Empty statements (standalone semicolons) parse correctly
features: [automatic-semicolon-insertion]
---*/

describe("ASI empty statements", () => {
  test("single semicolon is a valid empty statement", () => {
    ;
    expect(true).toBe(true);
  });

  test("multiple semicolons on separate lines", () => {
    ;
    ;
    ;
    ;
    expect(true).toBe(true);
  });

  test("multiple semicolons on one line", () => {
    ;;;;
    expect(true).toBe(true);
  });

  test("semicolons interleaved with expressions", () => {
    ;1;
    ;1
    ;1;
    ;1
    expect(true).toBe(true);
  });

  test("semicolons and expressions without line terminators", () => {
    ;;1;;1;;1;
    expect(true).toBe(true);
  });
});
