describe("implicit arguments object is materialized only when referenced", () => {
  test("function that never names arguments still binds its parameters", () => {
    function add(a, b) {
      return a + b;
    }

    expect(add(3, 4)).toBe(7);
  });

  test("direct reference still sees every passed argument", () => {
    function collect() {
      return [arguments.length, arguments[0], arguments[2]];
    }

    expect(collect(10, 20, 30)).toEqual([3, 10, 30]);
  });

  test("reference only inside a nested arrow keeps the enclosing arguments", () => {
    function viaArrow() {
      const read = () => arguments.length + ":" + arguments[1];
      return read();
    }

    expect(viaArrow("x", "y", "z")).toBe("3:y");
  });

  test("a nested non-arrow function has its own arguments", () => {
    function outer() {
      function inner() {
        return inner.name + arguments.length;
      }
      return inner(1, 2, 3, 4);
    }

    expect(outer("ignored")).toBe("inner4");
  });

  test("arguments referenced after several statements is still available", () => {
    function late(seed) {
      let total = seed;
      total += 1;
      total += 2;
      return total + arguments.length;
    }

    expect(late(100, "extra")).toBe(105);
  });

  test("elided and non-elided functions interleave correctly", () => {
    function plain(a) {
      return a * 2;
    }
    function counted() {
      return arguments.length;
    }

    expect(plain(5)).toBe(10);
    expect(counted(1, 2, 3)).toBe(3);
    expect(plain(counted(9))).toBe(2);
  });

  test("a Unicode-escaped arguments identifier is still materialized", () => {
    function viaEscape() {
      return \u0061rguments.length;
    }

    expect(viaEscape("a", "b")).toBe(2);
  });
});
