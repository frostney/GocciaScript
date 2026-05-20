/*---
description: ASI boundaries after with statement bodies
features: [automatic-semicolon-insertion, compat-non-strict-mode]
---*/

describe("with statement ASI", () => {
  test("inserts a semicolon after an expression body before a following block", () => {
    let hit = 0;

    with ({ value: 1 }) hit = value
    {
      const blockScoped = 2;
      hit += blockScoped;
    }

    expect(hit).toBe(3);
  });

  test("parses let as an expression body before a following block", () => {
    if (false) {
      with ({}) let
      {}
    }

    expect(true).toBe(true);
  });
});
