/*---
description: Prototype accessors observe the primitive receiver as this on primitive property access
features: [property-access, String.prototype, Number.prototype]
---*/

describe("prototype accessor receiver on primitive property access", () => {
  test("String.prototype getter sees the string primitive on computed access", () => {
    Object.defineProperty(String.prototype, "kindOfThis", {
      get() {
        return typeof this;
      },
      configurable: true,
    });
    try {
      const s = "abc";
      const k = "kindOfThis";
      expect(s[k]).toBe("string");
    } finally {
      delete String.prototype.kindOfThis;
    }
  });

  test("String.prototype getter sees the string primitive on static access", () => {
    Object.defineProperty(String.prototype, "kindOfThis", {
      get() {
        return typeof this;
      },
      configurable: true,
    });
    try {
      const s = "abc";
      expect(s.kindOfThis).toBe("string");
    } finally {
      delete String.prototype.kindOfThis;
    }
  });

  test("String.prototype getter receives the primitive value itself", () => {
    Object.defineProperty(String.prototype, "self", {
      get() {
        return this;
      },
      configurable: true,
    });
    try {
      const s = "abc";
      const k = "self";
      expect(s[k]).toBe("abc");
    } finally {
      delete String.prototype.self;
    }
  });

  test("Number.prototype getter sees the number primitive on computed access", () => {
    Object.defineProperty(Number.prototype, "kindOfThis", {
      get() {
        return typeof this;
      },
      configurable: true,
    });
    try {
      const n = 5;
      const k = "kindOfThis";
      expect(n[k]).toBe("number");
    } finally {
      delete Number.prototype.kindOfThis;
    }
  });

  test("symbol-keyed String.prototype getter sees the string primitive", () => {
    const key = Symbol("kindOfThis");
    Object.defineProperty(String.prototype, key, {
      get() {
        return typeof this;
      },
      configurable: true,
    });
    try {
      const s = "abc";
      expect(s[key]).toBe("string");
    } finally {
      delete String.prototype[key];
    }
  });
});
