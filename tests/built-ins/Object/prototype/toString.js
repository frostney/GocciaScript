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

    test("WeakSet returns [object WeakSet]", () => {
      expect(Object.prototype.toString.call(new WeakSet())).toBe("[object WeakSet]");
    });

    test("WeakMap returns [object WeakMap]", () => {
      expect(Object.prototype.toString.call(new WeakMap())).toBe("[object WeakMap]");
    });

    test("WeakRef returns [object WeakRef]", () => {
      expect(Object.prototype.toString.call(new WeakRef({}))).toBe("[object WeakRef]");
    });

    test("FinalizationRegistry returns [object FinalizationRegistry]", () => {
      expect(Object.prototype.toString.call(new FinalizationRegistry(() => {}))).toBe("[object FinalizationRegistry]");
    });

    test("Promise returns [object Promise]", () => {
      expect(Object.prototype.toString.call(new Promise((r) => r()))).toBe("[object Promise]");
    });

    test("ArrayBuffer returns [object ArrayBuffer]", () => {
      expect(Object.prototype.toString.call(new ArrayBuffer(8))).toBe("[object ArrayBuffer]");
    });

    test("Proxy wrapping Date does not expose Date builtin brand", () => {
      expect(Object.prototype.toString.call(new Proxy(new Date(0), {}))).toBe("[object Object]");
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

    test("built-in objects fall back to Object when Symbol.toStringTag is absent", () => {
      const mapTag = Map.prototype[Symbol.toStringTag];
      const setTag = Set.prototype[Symbol.toStringTag];

      delete Map.prototype[Symbol.toStringTag];
      delete Set.prototype[Symbol.toStringTag];

      try {
        expect(Object.prototype.toString.call(new Map())).toBe("[object Object]");
        expect(Object.prototype.toString.call(new Set())).toBe("[object Object]");
      } finally {
        Object.defineProperty(Map.prototype, Symbol.toStringTag, {
          value: mapTag,
          configurable: true,
        });
        Object.defineProperty(Set.prototype, Symbol.toStringTag, {
          value: setTag,
          configurable: true,
        });
      }
    });

    test("built-in objects fall back to Object for non-string Symbol.toStringTag", () => {
      const mapTag = Map.prototype[Symbol.toStringTag];
      Object.defineProperty(Map.prototype, Symbol.toStringTag, {
        value: new String("Map"),
        configurable: true,
      });

      try {
        expect(Object.prototype.toString.call(new Map())).toBe("[object Object]");
      } finally {
        Object.defineProperty(Map.prototype, Symbol.toStringTag, {
          value: mapTag,
          configurable: true,
        });
      }
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

    test("WeakMap and WeakSet with values return their built-in tags", () => {
      const key = {};
      expect(Object.prototype.toString.call(new WeakMap([[key, 1]]))).toBe("[object WeakMap]");
      expect(Object.prototype.toString.call(new WeakSet([key]))).toBe("[object WeakSet]");
    });

    test("WeakRef and FinalizationRegistry with values return their built-in tags", () => {
      expect(Object.prototype.toString.call(new WeakRef({}))).toBe("[object WeakRef]");
      expect(Object.prototype.toString.call(new FinalizationRegistry(() => {}))).toBe("[object FinalizationRegistry]");
    });
  });

  // ECMA-262 Object.prototype.toString: builtinTag comes only from the
  // internal slots it lists (Array, arguments, callable, Error, Boolean,
  // Number, String, Date, RegExp). Every other tag is @@toStringTag, read
  // through the prototype chain, so it goes away with the prototype.
  describe("builtinTag without the prototype", () => {
    const tagWithPrototype = (value, prototype) =>
      Object.prototype.toString.call(Object.setPrototypeOf(value, prototype));

    test("keeps the tags that come from internal slots", () => {
      expect(tagWithPrototype([], null)).toBe("[object Array]");
      expect(tagWithPrototype(() => {}, null)).toBe("[object Function]");
      expect(tagWithPrototype(new Error("x"), null)).toBe("[object Error]");
      expect(tagWithPrototype(Object(true), null)).toBe("[object Boolean]");
      expect(tagWithPrototype(Object(1), null)).toBe("[object Number]");
      expect(tagWithPrototype(Object("s"), null)).toBe("[object String]");
      expect(tagWithPrototype(/a/, null)).toBe("[object RegExp]");
    });

    test("a Date keeps its tag", () => {
      expect(tagWithPrototype(new Date(0), null)).toBe("[object Date]");
      expect(tagWithPrototype(new Date(0), {})).toBe("[object Date]");
      expect(tagWithPrototype(new Date(NaN), null)).toBe("[object Date]");
    });

    test("a Date subclass instance is a Date", () => {
      class LaterDate extends Date {}
      expect(tagWithPrototype(new LaterDate(0), null)).toBe("[object Date]");
    });

    test("an object that only inherits from Date.prototype is not a Date", () => {
      expect(Object.prototype.toString.call(Object.create(Date.prototype))).toBe(
        "[object Object]",
      );
      expect(Object.prototype.toString.call(Date.prototype)).toBe("[object Object]");
      expect(Object.prototype.toString.call(new Proxy(new Date(0), {}))).toBe(
        "[object Object]",
      );
    });

    test("an own @@toStringTag still wins on a Date", () => {
      const date = Object.setPrototypeOf(new Date(0), null);
      Object.defineProperty(date, Symbol.toStringTag, { value: "Moment" });
      expect(Object.prototype.toString.call(date)).toBe("[object Moment]");
    });

    test("a typed array loses its tag", () => {
      expect(tagWithPrototype(new Uint8Array(2), null)).toBe("[object Object]");
      expect(tagWithPrototype(new Float64Array(1), null)).toBe("[object Object]");
      expect(tagWithPrototype(new BigInt64Array(1), null)).toBe("[object Object]");
      expect(tagWithPrototype(new Uint8Array(2), {})).toBe("[object Object]");
    });

    test("an own @@toStringTag still wins on a typed array", () => {
      const typed = Object.setPrototypeOf(new Uint8Array(2), null);
      Object.defineProperty(typed, Symbol.toStringTag, { value: "Uint8Array" });
      expect(Object.prototype.toString.call(typed)).toBe("[object Uint8Array]");
    });

    test("a Temporal object loses its tag", () => {
      expect(tagWithPrototype(Temporal.Now.instant(), null)).toBe("[object Object]");
      expect(tagWithPrototype(Temporal.PlainDate.from("2020-01-01"), null)).toBe(
        "[object Object]",
      );
      expect(tagWithPrototype(Temporal.Duration.from({ days: 1 }), null)).toBe(
        "[object Object]",
      );
    });

    test("other built-in objects lose their tags", () => {
      expect(tagWithPrototype(new Map(), null)).toBe("[object Object]");
      expect(tagWithPrototype(new DataView(new ArrayBuffer(1)), null)).toBe(
        "[object Object]",
      );
      expect(tagWithPrototype(new ArrayBuffer(1), null)).toBe("[object Object]");
      expect(tagWithPrototype(Promise.resolve(), null)).toBe("[object Object]");
      expect(tagWithPrototype([1].values(), null)).toBe("[object Object]");
      expect(tagWithPrototype("a".matchAll(/a/g), null)).toBe("[object Object]");
    });
  });

  // WebIDL puts each interface's class string on its prototype as
  // @@toStringTag: { writable: false, enumerable: false, configurable: true }.
  describe("web platform objects", () => {
    const instances = [
      ["AbortController", () => new AbortController()],
      ["AbortSignal", () => new AbortController().signal],
      ["EventTarget", () => new EventTarget()],
      ["Event", () => new Event("x")],
      ["Headers", () => new Headers()],
      ["URL", () => new URL("http://example.com/")],
      ["URLSearchParams", () => new URLSearchParams("a=1")],
      ["Response", () => new Response("x")],
      ["TextEncoder", () => new TextEncoder()],
      ["TextDecoder", () => new TextDecoder()],
    ];

    for (const [name, make] of instances) {
      test(`${name} takes its tag from its prototype`, () => {
        const value = make();
        expect(Object.prototype.toString.call(value)).toBe(`[object ${name}]`);
        expect(
          Object.getOwnPropertyDescriptor(Object.getPrototypeOf(value), Symbol.toStringTag),
        ).toEqual({ value: name, writable: false, enumerable: false, configurable: true });
        Object.setPrototypeOf(value, null);
        expect(Object.prototype.toString.call(value)).toBe("[object Object]");
      });
    }
  });

  describe("Intl.Segmenter results", () => {
    test("a segments object has no tag of its own", () => {
      const segments = new Intl.Segmenter().segment("ab");
      expect(Object.prototype.toString.call(segments)).toBe("[object Object]");
    });

    test("a segment iterator is a Segmenter String Iterator", () => {
      const iterator = new Intl.Segmenter().segment("ab")[Symbol.iterator]();
      expect(Object.prototype.toString.call(iterator)).toBe(
        "[object Segmenter String Iterator]",
      );
      expect(
        Object.getOwnPropertyDescriptor(Object.getPrototypeOf(iterator), Symbol.toStringTag),
      ).toEqual({
        value: "Segmenter String Iterator",
        writable: false,
        enumerable: false,
        configurable: true,
      });
    });
  });
});
