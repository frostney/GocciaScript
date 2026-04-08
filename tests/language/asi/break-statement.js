/*---
description: ASI for break statements
features: [automatic-semicolon-insertion]
---*/

describe("ASI break statement", () => {
  test("break without semicolon in switch", () => {
    let result = "none";
    switch ("a") {
      case "a":
        result = "matched"
        break
      case "b":
        result = "wrong";
        break
    }
    expect(result).toBe("matched");
  });

  test("break before closing brace", () => {
    let result = "none";
    switch (1) {
      case 1:
        result = "one"
        break }
    expect(result).toBe("one");
  });
});
