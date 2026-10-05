/*---
description: With an expression in the parameter list, a body var named like a parameter is a separate binding, so closures created in the parameter list keep the parameter and the body keeps the var (ES2026 §10.2.11 FunctionDeclarationInstantiation)
features: [compat-function, compat-var, default-parameters, destructuring, rest-parameters, generators, async-functions, async-generators, class]
---*/

describe("body var environment when the parameter list has expressions", () => {
  test("an arrow function's default closure keeps the parameter", () => {
    const g = (a, f = () => a) => {
      var a = 5;
      return [a, f()];
    };
    expect(g(1)).toEqual([5, 1]);
  });

  test("a function declaration's default closure keeps the parameter", () => {
    function g(a, f = () => a) {
      var a = 5;
      return [a, f()];
    }
    expect(g(1)).toEqual([5, 1]);
  });

  test("a function expression's default closure keeps the parameter", () => {
    const g = function (a, f = () => a) {
      var a = 5;
      return [a, f()];
    };
    expect(g(1)).toEqual([5, 1]);
  });

  test("a later assignment in the body changes only the var", () => {
    const g = (a, f = () => a) => {
      var a;
      a = 9;
      return [a, f()];
    };
    expect(g(1)).toEqual([9, 1]);
  });

  test("a default closure that writes the parameter after the var is initialized leaves the var alone", () => {
    const g = (a, set = () => { a = 7; }, get = () => a) => {
      var a = 3;
      set();
      return [a, get()];
    };
    expect(g(1)).toEqual([3, 7]);
  });

  test("a closure created in the body sees the var", () => {
    function g(a, f = () => a) {
      var a = 5;
      const read = () => a;
      a = 6;
      return [read(), f()];
    }
    expect(g(1)).toEqual([6, 1]);
  });

  test("a body closure that writes the var leaves the parameter alone", () => {
    const g = (a, f = () => a) => {
      var a = 2;
      const write = () => { a = 8; };
      write();
      return [a, f()];
    };
    expect(g(1)).toEqual([8, 1]);
  });

  test("a parameter without a body var stays shared with the default closure", () => {
    const g = (a, f = () => a) => {
      a = 4;
      return [a, f()];
    };
    expect(g(1)).toEqual([4, 4]);
  });

  test("a var nested in a block, loop or pattern is the body's var too", () => {
    const block = (a, f = () => a) => {
      {
        var a = 5;
      }
      return [a, f()];
    };
    const forOf = (a, f = () => a) => {
      for (var a of [7]);
      return [a, f()];
    };
    const pattern = (a, f = () => a) => {
      var { a } = { a: 5 };
      return [a, f()];
    };
    expect(block(1)).toEqual([5, 1]);
    expect(forOf(1)).toEqual([7, 1]);
    expect(pattern(1)).toEqual([5, 1]);
  });

  test("a destructured parameter", () => {
    const g = ({ a }, f = () => a) => {
      var a = 5;
      return [a, f()];
    };
    expect(g({ a: 1 })).toEqual([5, 1]);
  });

  test("a parameter bound by a computed key", () => {
    const key = "a";
    const g = ({ [key]: a }, f = () => a) => {
      var a = 5;
      return [a, f()];
    };
    expect(g({ a: 1 })).toEqual([5, 1]);
  });

  test("a rest parameter", () => {
    const g = (f = () => rest, ...rest) => {
      var rest = [5];
      return [rest, f()];
    };
    expect(g(undefined, 1, 2)).toEqual([[5], [1, 2]]);
  });

  test("a body function declaration named like a parameter replaces only the body binding", () => {
    function g(a, f = () => a) {
      function a() {}
      return [typeof a, f()];
    }
    function h(a, f = () => a) {
      var a;
      function* a() {}
      return [typeof a, f()];
    }
    expect(g(1)).toEqual(["function", 1]);
    expect(h(1)).toEqual(["function", 1]);
  });

  test("a body var that is not a parameter starts as undefined", () => {
    const g = (a = Math.max(1, 2, 3)) => {
      var x;
      var y;
      var z;
      return [x, y, z];
    };
    function h({ p: [q, r] }, s = q) {
      var x;
      var y;
      var z;
      return [x, y, z];
    }
    expect(g()).toEqual([undefined, undefined, undefined]);
    expect(h({ p: [1, 2] })).toEqual([undefined, undefined, undefined]);
  });

  test("a body var that is not a parameter starts as undefined when the call passes surplus arguments", () => {
    function g(a, b = 1) {
      var x;
      var y;
      return [x, y];
    }
    expect(g(1, undefined, "surplus", "more")).toEqual([undefined, undefined]);
  });

  test("a default closure does not see a body var that is not a parameter", () => {
    const x = "outer";
    const g = (f = () => x) => {
      var x = "inner";
      return [x, f()];
    };
    expect(g()).toEqual(["inner", "outer"]);
  });

  test("object, class, static and constructor methods", () => {
    const object = {
      m(a, f = () => a) {
        var a = 5;
        return [a, f()];
      },
    };
    class C {
      constructor(a, f = () => a) {
        var a = 5;
        this.result = [a, f()];
      }
      m(a, f = () => a) {
        var a = 5;
        return [a, f()];
      }
      static s(a, f = () => a) {
        var a = 5;
        return [a, f()];
      }
    }
    class Base {}
    class Derived extends Base {
      constructor(a, f = () => a) {
        var a = 5;
        super();
        this.result = [a, f()];
      }
    }
    expect(object.m(1)).toEqual([5, 1]);
    expect(new C(1).result).toEqual([5, 1]);
    expect(new C(0).m(1)).toEqual([5, 1]);
    expect(C.s(1)).toEqual([5, 1]);
    expect(new Derived(1).result).toEqual([5, 1]);
  });

  test("object and class setters", () => {
    let result;
    const object = {
      set s([a, f = () => a]) {
        var a = 5;
        result = [a, f()];
      },
    };
    class C {
      static set s([a, f = () => a]) {
        var a = 5;
        result = [a, f()];
      }
    }
    object.s = [1];
    expect(result).toEqual([5, 1]);
    C.s = [2];
    expect(result).toEqual([5, 2]);
  });

  test("generator functions and methods", () => {
    function* g(a, f = () => a) {
      var a = 5;
      yield a;
      yield f();
    }
    const object = {
      *m(a, f = () => a) {
        var a = 5;
        yield [a, f()];
      },
    };
    expect([...g(1)]).toEqual([5, 1]);
    expect(object.m(1).next().value).toEqual([5, 1]);
  });

  test("async functions, arrows and methods", async () => {
    async function g(a, f = () => a) {
      var a = 5;
      await 0;
      return [a, f()];
    }
    const arrow = async (a, f = () => a) => {
      var a = 5;
      await 0;
      return [a, f()];
    };
    const object = {
      async m(a, f = () => a) {
        var a = 5;
        return [a, f()];
      },
    };
    expect(await g(1)).toEqual([5, 1]);
    expect(await arrow(1)).toEqual([5, 1]);
    expect(await object.m(1)).toEqual([5, 1]);
  });

  test("async generator functions and methods", async () => {
    async function* g(a, f = () => a) {
      var a = 5;
      yield [a, f()];
    }
    const object = {
      async *m(a, f = () => a) {
        var a = 5;
        yield [a, f()];
      },
    };
    expect((await g(1).next()).value).toEqual([5, 1]);
    expect((await object.m(1).next()).value).toEqual([5, 1]);
  });

  test("without a parameter expression the var is the parameter", () => {
    function g(a) {
      var a;
      return a;
    }
    const h = ({ a }) => {
      var a;
      return a;
    };
    expect(g(1)).toBe(1);
    expect(h({ a: 2 })).toBe(2);
  });
});
