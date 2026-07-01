describe("arguments object", () => {
  test("sloppy simple parameters receive a mapped arguments object", () => {
    function capture(a) {
      arguments[0] = 9;
      const afterArgumentWrite = a;
      a = 7;
      return [arguments.length, arguments[0], arguments[1], afterArgumentWrite, a];
    }

    expect(capture(1, 2)).toEqual([2, 7, 2, 9, 7]);
  });

  test("explicit arguments flag coexists with sloppy this binding", () => {
    function capture(first) {
      return [arguments.length, arguments[0], first, this === globalThis];
    }

    expect(capture(11)).toEqual([1, 11, 11, true]);
  });

  test("arguments object iterator matches Array prototype iterator", () => {
    function mapped(first) {
      return Object.getOwnPropertyDescriptor(arguments, Symbol.iterator).value;
    }

    function unmapped(first = "default") {
      return Object.getOwnPropertyDescriptor(arguments, Symbol.iterator).value;
    }

    expect(mapped("value")).toBe([][Symbol.iterator]);
    expect(unmapped("value")).toBe([][Symbol.iterator]);
  });

  test("Object.prototype.toString brands arguments objects", () => {
    function capture() {
      return Object.prototype.toString.call(arguments);
    }

    expect(capture()).toBe("[object Arguments]");
  });

  test("simple parameter arguments alias parameter bindings", () => {
    function capture(first, second) {
      const before = [arguments[0], first, arguments[1], second];
      first = 20;
      arguments[1] = 30;
      return [before, arguments[0], first, arguments[1], second];
    }

    expect(capture(1, 2)).toEqual([[1, 1, 2, 2], 20, 20, 30, 30]);
  });

  test("mapped parameter bindings stay current after increment operations", () => {
    function increment(first) {
      first++;
      return [first, arguments[0]];
    }

    function decrement(first) {
      --first;
      return [first, arguments[0]];
    }

    expect(increment(1)).toEqual([2, 2]);
    expect(decrement(3)).toEqual([2, 2]);
  });

  test("deleting an indexed property breaks the parameter mapping", () => {
    function capture(first) {
      delete arguments[0];
      first = 20;
      return [arguments[0], first];
    }

    expect(capture(1)).toEqual([undefined, 20]);
  });

  test("making an indexed property non-writable breaks the parameter mapping", () => {
    function capture(first) {
      Object.defineProperty(arguments, "0", { value: 5, writable: false });
      first = 20;
      return [arguments[0], first];
    }

    expect(capture(1)).toEqual([5, 20]);
  });

  test("converting an indexed property to an accessor breaks the parameter mapping", () => {
    function capture(first) {
      Object.defineProperty(arguments, "0", {
        get() {
          return 44;
        }
      });
      first = 20;
      return [arguments[0], first];
    }

    expect(capture(1)).toEqual([44, 20]);
  });

  test("non-simple parameter lists receive unmapped arguments objects", () => {
    function withDefault(first = "default") {
      first = "changed";
      arguments[0] = "argument";
      return [first, arguments[0]];
    }

    function withRest(first, ...rest) {
      first = "changed";
      arguments[0] = "argument";
      return [first, arguments[0], rest[0]];
    }

    expect(withDefault("value")).toEqual(["changed", "argument"]);
    expect(withRest("value", "rest")).toEqual(["changed", "argument", "rest"]);
  });

  test("arrows capture arguments from the nearest ordinary function", () => {
    function outer() {
      const pick = () => arguments[1];
      return pick();
    }

    expect(outer("first", "second")).toBe("second");
  });

  test("a parameter named arguments shadows the implicit object", () => {
    function echo(arguments) {
      return arguments;
    }

    expect(echo(42)).toBe(42);
  });

  test("a body lexical declaration named arguments shadows the implicit object", () => {
    function pick() {
      let arguments = "body";
      return arguments;
    }

    expect(pick("call")).toBe("body");
  });

  test("coexists with default and rest parameters", () => {
    function withDefault(first = arguments.length) {
      return first;
    }

    function withRest(...items) {
      return [arguments.length, arguments[1], items[2]];
    }

    expect(withDefault()).toBe(0);
    expect(withDefault(undefined)).toBe(1);
    expect(withRest(1, 2, 3)).toEqual([3, 2, 3]);
  });

  test("generators receive arguments objects", () => {
    function* gen(value) {
      arguments[0] = 8;
      yield arguments[0];
      yield value;
      value = 9;
      yield arguments[0];
      yield arguments.length;
    }

    const iterator = gen(7);

    expect(iterator.next().value).toBe(8);
    expect(iterator.next().value).toBe(8);
    expect(iterator.next().value).toBe(9);
    expect(iterator.next().value).toBe(1);
  });

  test("generator yield delegation sees updated mapped parameters", () => {
    function* countdown(n) {
      if (n > 0) {
        yield n;
        yield* countdown(--n);
      }
      return 34;
    }

    const iterator = countdown(3);
    expect(iterator.next()).toEqual({ value: 3, done: false });
    expect(iterator.next()).toEqual({ value: 2, done: false });
    expect(iterator.next()).toEqual({ value: 1, done: false });
    expect(iterator.next()).toEqual({ value: 34, done: true });
  });

  test("generator methods receive arguments objects", () => {
    const obj = {
      *gen(value) {
        yield arguments.length + arguments[0];
      }
    };

    expect(obj.gen(4).next().value).toBe(5);
  });

  test("methods and accessors receive arguments objects", () => {
    class Box {
      method(value) {
        return arguments.length + arguments[0];
      }

      get emptyCount() {
        return arguments.length;
      }

      set stored(value) {
        this.value = arguments[0];
      }
    }

    const box = new Box();
    box.stored = 8;

    expect(box.method(4)).toBe(5);
    expect(box.emptyCount).toBe(0);
    expect(box.value).toBe(8);
  });

  test("object literal accessors receive arguments objects", () => {
    const obj = {
      get count() {
        return arguments.length;
      },

      set value(next) {
        this.seen = arguments[0];
      }
    };

    obj.value = 6;

    expect(obj.count).toBe(0);
    expect(obj.seen).toBe(6);
  });

  test("callee accessors throw on unmapped arguments objects", () => {
    function getCallee() {
      "use strict";
      return arguments.callee;
    }

    function setCallee() {
      "use strict";
      arguments.callee = function replacement() {};
    }

    expect(() => getCallee()).toThrow(TypeError);
    expect(() => setCallee()).toThrow(TypeError);
  });
});
