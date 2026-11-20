/*---
description: In a JavaScript file a template between angle brackets after a constructor is a comparison, not type arguments
features: [types-as-comments]
---*/

class Box {
  valueOf() {
    return 0;
  }
}

describe("template comparisons in JavaScript", () => {
  test("a constructor compared with a template stays a comparison", () => {
    const b = 1;

    // prettier-ignore
    expect(new Box < `${b}` > (0)).toBe(true);
  });
});
