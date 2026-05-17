describe("arguments object", () => {
  test("ordinary functions receive an unmapped arguments object", () => {
    function capture(a) {
      arguments[0] = 9;
      a = 7;
      return [arguments.length, arguments[0], arguments[1], a];
    }

    expect(capture(1, 2)).toEqual([2, 9, 2, 7]);
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
      yield arguments[0];
      yield arguments.length;
    }

    const iterator = gen(7);

    expect(iterator.next().value).toBe(7);
    expect(iterator.next().value).toBe(1);
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
      return arguments.callee;
    }

    function setCallee() {
      arguments.callee = function replacement() {};
    }

    expect(() => getCallee()).toThrow(TypeError);
    expect(() => setCallee()).toThrow(TypeError);
  });
});
