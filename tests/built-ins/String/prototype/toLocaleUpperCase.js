/*---
description: String.prototype.toLocaleUpperCase basic functionality
features: [String.prototype.toLocaleUpperCase]
---*/

describe("String.prototype.toLocaleUpperCase", () => {
  test("exists and maps through Unicode uppercase conversion", () => {
    expect("hello".toLocaleUpperCase()).toBe("HELLO");
    expect("éöσ".toLocaleUpperCase()).toBe("ÉÖΣ");
  });

  test("coerces non-string receivers", () => {
    expect(String.prototype.toLocaleUpperCase.call(true)).toBe("TRUE");
  });

  test("has function metadata", () => {
    expect(String.prototype.toLocaleUpperCase.name).toBe("toLocaleUpperCase");
    expect(String.prototype.toLocaleUpperCase.length).toBe(0);
  });
});
