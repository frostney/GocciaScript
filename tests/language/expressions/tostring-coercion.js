describe("ToString coercion via ToPrimitive", () => {
  describe("template literals call ToString (string hint)", () => {
    test("custom toString on object", () => {
      const obj = { toString() { return "hello"; } };
      expect(`${obj}`).toBe("hello");
    });

    test("string hint: toString preferred over valueOf", () => {
      const obj = { valueOf() { return 10; }, toString() { return "str"; } };
      expect(`${obj}`).toBe("str");
    });

    test("toString returns non-primitive, falls back to valueOf", () => {
      const obj = { valueOf() { return "val"; }, toString() { return {}; } };
      expect(`${obj}`).toBe("val");
    });

    test("neither returns primitive throws TypeError", () => {
      const obj = { valueOf() { return {}; }, toString() { return {}; } };
      expect(() => `${obj}`).toThrow(TypeError);
    });

    test("class instance with toString method", () => {
      class Point {
        constructor(x, y) {
          this.x = x;
          this.y = y;
        }
        toString() {
          return `(${this.x}, ${this.y})`;
        }
      }
      const p = new Point(1, 2);
      expect(`${p}`).toBe("(1, 2)");
    });
  });

  describe("Array.prototype.join calls ToString (string hint)", () => {
    test("custom toString on elements", () => {
      const obj = { toString() { return "obj"; } };
      expect([obj, 1].join(",")).toBe("obj,1");
    });

    test("string hint: toString preferred over valueOf", () => {
      const obj = { valueOf() { return 99; }, toString() { return "str"; } };
      expect([obj].join(",")).toBe("str");
    });

    test("class instances as array elements", () => {
      class Tag {
        constructor(name) { this.name = name; }
        toString() { return `<${this.name}>`; }
      }
      expect([new Tag("a"), new Tag("b")].join(", ")).toBe("<a>, <b>");
    });

    test("null and undefined are empty strings in join", () => {
      expect([null, undefined, 1].join(",")).toBe(",,1");
    });

    test("nested arrays use join recursively", () => {
      expect([1, [2, 3], 4].join(",")).toBe("1,2,3,4");
    });
  });

  describe("Array.prototype.toString delegates to join", () => {
    test("custom toString on elements", () => {
      const obj = { toString() { return "custom"; } };
      expect([obj, "a"].toString()).toBe("custom,a");
    });

    test("used in string concatenation", () => {
      const obj = { toString() { return "x"; } };
      expect("" + [obj, 1]).toBe("x,1");
    });
  });

  describe("addition operator uses default hint", () => {
    test("valueOf preferred for objects with both", () => {
      const obj = { valueOf() { return 10; }, toString() { return "str"; } };
      expect(obj + 1).toBe(11);
    });

    test("valueOf returns non-primitive, falls back to toString", () => {
      const obj = { valueOf() { return {}; }, toString() { return "fallback"; } };
      expect(obj + "!").toBe("fallback!");
    });

    test("string concatenation with custom toString", () => {
      const obj = { toString() { return "world"; } };
      expect("hello " + obj).toBe("hello world");
    });
  });

  describe("Symbol coercion errors", () => {
    test("Symbol in template literal throws TypeError", () => {
      const sym = Symbol("x");
      expect(() => `${sym}`).toThrow(TypeError);
    });

    test("Symbol in string concatenation throws TypeError", () => {
      const sym = Symbol("x");
      expect(() => "" + sym).toThrow(TypeError);
    });
  });

  describe("edge cases", () => {
    test("null in template literal", () => {
      expect(`${null}`).toBe("null");
    });

    test("undefined in template literal", () => {
      expect(`${undefined}`).toBe("undefined");
    });

    test("NaN in template literal", () => {
      expect(`${NaN}`).toBe("NaN");
    });

    test("Infinity in template literal", () => {
      expect(`${Infinity}`).toBe("Infinity");
    });

    test("boolean in template literal", () => {
      expect(`${true}`).toBe("true");
      expect(`${false}`).toBe("false");
    });

    test("empty object in template literal uses toString", () => {
      expect(`${{}}`).toBe("[object Object]");
    });

    test("array in template literal", () => {
      expect(`${[1, 2, 3]}`).toBe("1,2,3");
    });

    test("empty array in template literal", () => {
      expect(`${[]}`).toBe("");
    });

    test("toString returning empty string", () => {
      const obj = { toString() { return ""; } };
      expect(`${obj}`).toBe("");
      expect([obj].join(",")).toBe("");
    });

    test("toString returning number string", () => {
      const obj = { toString() { return "42"; } };
      expect(`${obj}`).toBe("42");
      expect([obj].join(",")).toBe("42");
    });

    test("multiple interpolations in template", () => {
      const a = { toString() { return "A"; } };
      const b = { toString() { return "B"; } };
      expect(`${a} and ${b}`).toBe("A and B");
    });
  });
});
