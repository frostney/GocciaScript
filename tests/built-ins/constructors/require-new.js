describe("class constructors require 'new'", () => {
  describe("built-in constructors throw TypeError without new", () => {
    test("Map() throws TypeError", () => {
      // biome-ignore lint/correctness/noInvalidBuiltinInstantiation: intentional - testing that Map() without new throws
      expect(() => Map()).toThrow(TypeError);
    });

    test("Set() throws TypeError", () => {
      // biome-ignore lint/correctness/noInvalidBuiltinInstantiation: intentional - testing that Set() without new throws
      expect(() => Set()).toThrow(TypeError);
    });

    test("ArrayBuffer() throws TypeError", () => {
      // biome-ignore lint/correctness/noInvalidBuiltinInstantiation: intentional - testing that ArrayBuffer() without new throws
      expect(() => ArrayBuffer(8)).toThrow(TypeError);
    });

    test("SharedArrayBuffer() throws TypeError", () => {
      // biome-ignore lint/correctness/noInvalidBuiltinInstantiation: intentional - testing that SharedArrayBuffer() without new throws
      expect(() => SharedArrayBuffer(8)).toThrow(TypeError);
    });
  });

  describe("user-defined classes throw TypeError without new", () => {
    test("empty class throws TypeError", () => {
      class Foo {}
      expect(() => Foo()).toThrow(TypeError);
    });

    test("class with constructor throws TypeError", () => {
      class Bar {
        constructor(x) {
          this.x = x;
        }
      }
      expect(() => Bar(42)).toThrow(TypeError);
    });

    test("class extending another class throws TypeError", () => {
      class Base {}
      class Child extends Base {}
      expect(() => Child()).toThrow(TypeError);
    });

    test("error message includes class name", () => {
      class MyClass {}
      try {
        MyClass();
        expect(true).toBe(false);
      } catch (e) {
        expect(e.name).toBe("TypeError");
        expect(e.message.includes("MyClass")).toBe(true);
        expect(e.message.includes("new")).toBe(true);
      }
    });
  });

  describe("constructors that work without new (type conversion)", () => {
    test("String() returns a string primitive", () => {
      expect(String(42)).toBe("42");
      expect(String(true)).toBe("true");
      expect(String(null)).toBe("null");
      expect(String(undefined)).toBe("undefined");
      expect(String()).toBe("");
      expect(typeof String(42)).toBe("string");
    });

    test("Number() returns a number primitive", () => {
      expect(Number("42")).toBe(42);
      expect(Number(true)).toBe(1);
      expect(Number(false)).toBe(0);
      expect(Number(null)).toBe(0);
      expect(Number()).toBe(0);
      expect(typeof Number("42")).toBe("number");
    });

    test("Boolean() returns a boolean primitive", () => {
      expect(Boolean(1)).toBe(true);
      expect(Boolean(0)).toBe(false);
      expect(Boolean("")).toBe(false);
      expect(Boolean("hello")).toBe(true);
      expect(Boolean(null)).toBe(false);
      expect(Boolean()).toBe(false);
      expect(typeof Boolean(1)).toBe("boolean");
    });
  });

  describe("constructors that work without new (same as new)", () => {
    test("Array() creates an array", () => {
      const a = Array();
      expect(a instanceof Array).toBe(true);
      expect(a.length).toBe(0);
    });

    test("Array(3) creates an array with length", () => {
      const a = Array(3);
      expect(a.length).toBe(3);
    });

    test("Object() creates a plain object", () => {
      const o = Object();
      expect(typeof o).toBe("object");
    });
  });

  describe("new still works for all constructors", () => {
    test("new Map()", () => {
      const m = new Map();
      expect(m instanceof Map).toBe(true);
    });

    test("new Set()", () => {
      const s = new Set();
      expect(s instanceof Set).toBe(true);
    });

    test("new ArrayBuffer(4)", () => {
      const buf = new ArrayBuffer(4);
      expect(buf instanceof ArrayBuffer).toBe(true);
      expect(buf.byteLength).toBe(4);
    });

    test("new SharedArrayBuffer(4)", () => {
      const sab = new SharedArrayBuffer(4);
      expect(sab instanceof SharedArrayBuffer).toBe(true);
      expect(sab.byteLength).toBe(4);
    });

    test("new with user-defined class", () => {
      class Baz {
        constructor(v) { this.v = v; }
      }
      const b = new Baz(10);
      expect(b instanceof Baz).toBe(true);
      expect(b.v).toBe(10);
    });
  });
});
