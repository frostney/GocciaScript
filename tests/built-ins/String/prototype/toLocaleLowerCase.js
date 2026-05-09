/*---
description: String.prototype.toLocaleLowerCase basic functionality
features: [String.prototype.toLocaleLowerCase]
---*/

const hasICU = typeof Intl !== "undefined" && new Intl.NumberFormat("en").format(1000).includes(",");

describe("String.prototype.toLocaleLowerCase", () => {
  test("exists and maps through Unicode lowercase conversion", () => {
    expect("HELLO".toLocaleLowerCase()).toBe("hello");
    if (hasICU) {
      expect("ÉÖΣ".toLocaleLowerCase()).toBe("éöς");
    } else {
      expect("ÉÖΣ".toLocaleLowerCase()).toBe("éöσ");
    }
  });

  test("coerces non-string receivers", () => {
    expect(String.prototype.toLocaleLowerCase.call(true)).toBe("true");
    expect(() => String.prototype.toLocaleLowerCase.call(null)).toThrow(
      TypeError
    );
    expect(() => String.prototype.toLocaleLowerCase.call(undefined)).toThrow(
      TypeError
    );
  });

  test("has function metadata", () => {
    expect(String.prototype.toLocaleLowerCase.name).toBe("toLocaleLowerCase");
    expect(String.prototype.toLocaleLowerCase.length).toBe(0);
  });
});
