/*---
description: String.prototype.trim works correctly
features: [String.prototype.trim]
---*/

describe("String.prototype.trim", () => {
  test("removes leading and trailing whitespace", () => {
    expect("  hello world  ".trim()).toBe("hello world");
    expect("  hello world".trim()).toBe("hello world");
    expect("hello world  ".trim()).toBe("hello world");
  });

  test("does not remove interior whitespace", () => {
    expect("hello   world".trim()).toBe("hello   world");
  });

  test("removes tabs and newlines", () => {
    expect("\thello\t".trim()).toBe("hello");
    expect("\nhello\n".trim()).toBe("hello");
  });

  test("empty and whitespace-only strings", () => {
    expect("".trim()).toBe("");
    expect(" ".trim()).toBe("");
    expect("  ".trim()).toBe("");
  });

  test("string with no whitespace unchanged", () => {
    expect("hello".trim()).toBe("hello");
  });
});
