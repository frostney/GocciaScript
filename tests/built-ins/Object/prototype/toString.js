describe("Object.prototype.toString", () => {
  test("exists on Object.prototype", () => {
    expect(typeof Object.prototype.toString).toBe("function");
  });

  describe("primitive values via .call()", () => {
    test("undefined returns [object Undefined]", () => {
      expect(Object.prototype.toString.call(undefined)).toBe("[object Undefined]");
    });

    test("null returns [object Null]", () => {
      expect(Object.prototype.toString.call(null)).toBe("[object Null]");
    });

    test("boolean true returns [object Boolean]", () => {
      expect(Object.prototype.toString.call(true)).toBe("[object Boolean]");
    });

    test("boolean false returns [object Boolean]", () => {
      expect(Object.prototype.toString.call(false)).toBe("[object Boolean]");
    });

    test("number returns [object Number]", () => {
      expect(Object.prototype.toString.call(42)).toBe("[object Number]");
    });

    test("zero returns [object Number]", () => {
      expect(Object.prototype.toString.call(0)).toBe("[object Number]");
    });

    test("NaN returns [object Number]", () => {
      expect(Object.prototype.toString.call(NaN)).toBe("[object Number]");
    });

    test("Infinity returns [object Number]", () => {
      expect(Object.prototype.toString.call(Infinity)).toBe("[object Number]");
    });

    test("string returns [object String]", () => {
      expect(Object.prototype.toString.call("hello")).toBe("[object String]");
    });

    test("empty string returns [object String]", () => {
      expect(Object.prototype.toString.call("")).toBe("[object String]");
    });

    test("symbol returns [object Symbol]", () => {
      expect(Object.prototype.toString.call(Symbol("x"))).toBe("[object Symbol]");
    });

    test("Symbol.iterator returns [object Symbol]", () => {
      expect(Object.prototype.toString.call(Symbol.iterator)).toBe("[object Symbol]");
    });
  });

  describe("built-in objects via .call()", () => {
    test("plain object returns [object Object]", () => {
      expect(Object.prototype.toString.call({})).toBe("[object Object]");
    });

    test("array returns [object Array]", () => {
      expect(Object.prototype.toString.call([])).toBe("[object Array]");
      expect(Object.prototype.toString.call([1, 2, 3])).toBe("[object Array]");
    });

    test("function returns [object Function]", () => {
      expect(Object.prototype.toString.call(() => {})).toBe("[object Function]");
    });

    test("Set returns [object Set]", () => {
      expect(Object.prototype.toString.call(new Set())).toBe("[object Set]");
    });

    test("Map returns [object Map]", () => {
      expect(Object.prototype.toString.call(new Map())).toBe("[object Map]");
    });

    test("Promise returns [object Promise]", () => {
      expect(Object.prototype.toString.call(new Promise((r) => r()))).toBe("[object Promise]");
    });

    test("ArrayBuffer returns [object ArrayBuffer]", () => {
      expect(Object.prototype.toString.call(new ArrayBuffer(8))).toBe("[object ArrayBuffer]");
    });
  });

  describe("Symbol.toStringTag override", () => {
    test("custom toStringTag on plain object", () => {
      const obj = { [Symbol.toStringTag]: "MyCustomType" };
      expect(Object.prototype.toString.call(obj)).toBe("[object MyCustomType]");
    });

    test("custom toStringTag overrides default tag", () => {
      const arr = [1, 2, 3];
      arr[Symbol.toStringTag] = "NotAnArray";
      expect(Object.prototype.toString.call(arr)).toBe("[object NotAnArray]");
    });

    test("non-string toStringTag is ignored", () => {
      const obj = { [Symbol.toStringTag]: 42 };
      expect(Object.prototype.toString.call(obj)).toBe("[object Object]");
    });

    test("null toStringTag is ignored", () => {
      const obj = { [Symbol.toStringTag]: null };
      expect(Object.prototype.toString.call(obj)).toBe("[object Object]");
    });

    test("undefined toStringTag is ignored", () => {
      const obj = { [Symbol.toStringTag]: undefined };
      expect(Object.prototype.toString.call(obj)).toBe("[object Object]");
    });

    test("empty string toStringTag", () => {
      const obj = { [Symbol.toStringTag]: "" };
      expect(Object.prototype.toString.call(obj)).toBe("[object ]");
    });
  });

  describe("direct invocation on objects", () => {
    test("calling toString directly on plain object", () => {
      const obj = {};
      expect(obj.toString()).toBe("[object Object]");
    });

    test("inherited toString from Object.prototype", () => {
      const obj = Object.create(null);
      expect(Object.prototype.toString.call(obj)).toBe("[object Object]");
    });
  });

  describe("class instances", () => {
    test("class instance returns [object Object] by default", () => {
      class Foo {}
      const f = new Foo();
      expect(Object.prototype.toString.call(f)).toBe("[object Object]");
    });

    test("class instance with Symbol.toStringTag property", () => {
      class Bar {}
      const b = new Bar();
      b[Symbol.toStringTag] = "Bar";
      expect(Object.prototype.toString.call(b)).toBe("[object Bar]");
    });

    test("class with Symbol.toStringTag getter", () => {
      class Baz {
        get [Symbol.toStringTag]() {
          return "Baz";
        }
      }
      const b = new Baz();
      expect(Object.prototype.toString.call(b)).toBe("[object Baz]");
      expect(b[Symbol.toStringTag]).toBe("Baz");
    });
  });

  describe("Symbol.toStringTag edge cases", () => {
    test("boolean toStringTag is ignored", () => {
      const obj = { [Symbol.toStringTag]: true };
      expect(Object.prototype.toString.call(obj)).toBe("[object Object]");
    });

    test("number toStringTag is ignored", () => {
      const obj = { [Symbol.toStringTag]: 42 };
      expect(Object.prototype.toString.call(obj)).toBe("[object Object]");
    });

    test("object toStringTag is ignored", () => {
      const obj = { [Symbol.toStringTag]: {} };
      expect(Object.prototype.toString.call(obj)).toBe("[object Object]");
    });

    test("array toStringTag is ignored", () => {
      const obj = { [Symbol.toStringTag]: ["Foo"] };
      expect(Object.prototype.toString.call(obj)).toBe("[object Object]");
    });

    test("function toStringTag is ignored", () => {
      const obj = { [Symbol.toStringTag]: () => "Foo" };
      expect(Object.prototype.toString.call(obj)).toBe("[object Object]");
    });

    test("Symbol.toStringTag on inherited prototype", () => {
      class Base {
        get [Symbol.toStringTag]() {
          return "Base";
        }
      }
      class Child extends Base {}
      expect(Object.prototype.toString.call(new Child())).toBe("[object Base]");
    });

    test("child class can override parent Symbol.toStringTag", () => {
      class Parent {
        get [Symbol.toStringTag]() {
          return "Parent";
        }
      }
      class Child extends Parent {
        get [Symbol.toStringTag]() {
          return "Child";
        }
      }
      expect(Object.prototype.toString.call(new Parent())).toBe("[object Parent]");
      expect(Object.prototype.toString.call(new Child())).toBe("[object Child]");
    });
  });

  describe("negative number edge cases", () => {
    test("-0 returns [object Number]", () => {
      expect(Object.prototype.toString.call(-0)).toBe("[object Number]");
    });

    test("-Infinity returns [object Number]", () => {
      expect(Object.prototype.toString.call(-Infinity)).toBe("[object Number]");
    });
  });

  describe("additional built-in types", () => {
    test("SharedArrayBuffer returns [object SharedArrayBuffer]", () => {
      expect(Object.prototype.toString.call(new SharedArrayBuffer(4))).toBe("[object SharedArrayBuffer]");
    });

    test("Set with values returns [object Set]", () => {
      expect(Object.prototype.toString.call(new Set([1, 2]))).toBe("[object Set]");
    });

    test("Map with entries returns [object Map]", () => {
      expect(Object.prototype.toString.call(new Map([["a", 1]]))).toBe("[object Map]");
    });
  });
});
