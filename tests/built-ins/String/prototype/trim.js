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

  test("removes ECMAScript Unicode whitespace", () => {
    const whitespace = "\u00A0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u2028\u2029\u202F\u205F\u3000\uFEFF";
    expect((whitespace + "hello" + whitespace).trim()).toBe("hello");
  });

  test("coerces non-string receivers", () => {
    expect(String.prototype.trim.call(42)).toBe("42");
    expect(String.prototype.trim.call(true)).toBe("true");
  });

  test("throws for nullish receivers", () => {
    expect(() => String.prototype.trim.call(null)).toThrow(TypeError);
    expect(() => String.prototype.trim.call(undefined)).toThrow(TypeError);
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
