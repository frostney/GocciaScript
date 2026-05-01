describe("Symbol.species", () => {
  test("Symbol.species is a symbol", () => {
    expect(typeof Symbol.species).toBe("symbol");
  });

  test("Array[Symbol.species] returns Array constructor", () => {
    expect(Array[Symbol.species]).toBe(Array);
  });

  test("Map[Symbol.species] returns Map constructor", () => {
    expect(Map[Symbol.species]).toBe(Map);
  });

  test("Set[Symbol.species] returns Set constructor", () => {
    expect(Set[Symbol.species]).toBe(Set);
  });

  test("Promise[Symbol.species] returns Promise constructor", () => {
    expect(Promise[Symbol.species]).toBe(Promise);
  });

  test("RegExp[Symbol.species] returns RegExp constructor", () => {
    expect(RegExp[Symbol.species]).toBe(RegExp);
  });

  test("Symbol.species accessor names", () => {
    expect(Object.getOwnPropertyDescriptor(Array, Symbol.species).get.name).toBe("get [Symbol.species]");
    expect(Object.getOwnPropertyDescriptor(Map, Symbol.species).get.name).toBe("get [Symbol.species]");
    expect(Object.getOwnPropertyDescriptor(Promise, Symbol.species).get.name).toBe("get [Symbol.species]");
    expect(Object.getOwnPropertyDescriptor(RegExp, Symbol.species).get.name).toBe("get [Symbol.species]");
    expect(Object.getOwnPropertyDescriptor(Set, Symbol.species).get.name).toBe("get [Symbol.species]");
  });

  test("RegExp subclass inherits Symbol.species through constructor prototype chain", () => {
    class MyRegExp extends RegExp {}
    expect(MyRegExp[Symbol.species]).toBe(MyRegExp);
  });

  test("RegExp subclass constructs native RegExp instances", () => {
    class MyRegExp extends RegExp {}
    const regex = new MyRegExp("a+");
    expect(regex.test("aa")).toBe(true);
    expect(regex.test("bb")).toBe(false);
  });

  test("RegExp subclass explicit constructor reuses native super receiver", () => {
    class MyRegExp extends RegExp {
      constructor(pattern) {
        super(pattern);
        this.extra = "ok";
      }
    }

    const regex = new MyRegExp("a+");
    expect(regex.test("aa")).toBe(true);
    expect(regex.extra).toBe("ok");
  });

  test("RegExp subclass resolves native super methods", () => {
    class MyRegExp extends RegExp {
      getTestMethodType() {
        return typeof super.test;
      }

      static getEscapeType() {
        return typeof super.escape;
      }

      get testMethodType() {
        return typeof super.test;
      }

      static get escapeType() {
        return typeof super.escape;
      }
    }

    expect(new MyRegExp("a").getTestMethodType()).toBe("function");
    expect(MyRegExp.getEscapeType()).toBe("function");
    expect(new MyRegExp("a").testMethodType).toBe("function");
    expect(MyRegExp.escapeType).toBe("function");
  });

  test("RegExp subclass resolves computed native super methods", () => {
    const instanceKey = "test";
    const staticKey = "escape";
    class MyRegExp extends RegExp {
      getComputedTestMethodType() {
        return typeof super[instanceKey];
      }

      matchesWithComputedSuper(value) {
        return super[instanceKey](value);
      }

      static getComputedEscapeType() {
        return typeof super[staticKey];
      }

      get computedTestMethodType() {
        return typeof super[instanceKey];
      }

      static get computedEscapeType() {
        return typeof super[staticKey];
      }
    }

    const regex = new MyRegExp("a+");
    expect(regex.getComputedTestMethodType()).toBe("function");
    expect(regex.matchesWithComputedSuper("aa")).toBe(true);
    expect(regex.computedTestMethodType).toBe("function");
    expect(MyRegExp.getComputedEscapeType()).toBe("function");
    expect(MyRegExp.computedEscapeType).toBe("function");
  });

  test("Promise subclass inherits Symbol.species through constructor prototype chain", () => {
    class MyPromise extends Promise {}
    expect(MyPromise[Symbol.species]).toBe(MyPromise);
  });

  test("native constructor superclass prototype must be an object or null", () => {
    const originalDescriptor = Object.getOwnPropertyDescriptor(RegExp, "prototype");
    try {
      Object.defineProperty(RegExp, "prototype", { value: null, configurable: true });
      class NullPrototypeRegExp extends RegExp {}
      expect(Object.getPrototypeOf(NullPrototypeRegExp.prototype)).toBe(null);

      Object.defineProperty(RegExp, "prototype", { value: 1, configurable: true });
      expect(() => { class InvalidPrototypeRegExp extends RegExp {} }).toThrow(TypeError);
    } finally {
      Object.defineProperty(RegExp, "prototype", originalDescriptor);
    }
  });
});
