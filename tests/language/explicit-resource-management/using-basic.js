describe("using declaration", () => {
  test("calls [Symbol.dispose] at block exit", () => {
    let disposed = false;
    {
      using resource = {
        [Symbol.dispose]() {
          disposed = true;
        }
      };
      expect(disposed).toBe(false);
    }
    expect(disposed).toBe(true);
  });

  test("disposes multiple resources in reverse order", () => {
    const order = [];
    {
      using a = {
        [Symbol.dispose]() { order.push("a"); }
      };
      using b = {
        [Symbol.dispose]() { order.push("b"); }
      };
      using c = {
        [Symbol.dispose]() { order.push("c"); }
      };
    }
    expect(order).toEqual(["c", "b", "a"]);
  });

  test("binds the value as const", () => {
    let value;
    {
      using resource = {
        val: 42,
        [Symbol.dispose]() {}
      };
      value = resource.val;
    }
    expect(value).toBe(42);
  });

  test("later lexical binding is in TDZ inside using block", () => {
    let value = "outer";
    {
      using resource = {
        [Symbol.dispose]() {}
      };
      expect(() => value).toThrow(ReferenceError);
      let value = "inner";
      expect(value).toBe("inner");
    }
    expect(value).toBe("outer");
  });

  test("using null is silently skipped", () => {
    let reached = false;
    {
      using resource = null;
      reached = true;
    }
    expect(reached).toBe(true);
  });

  test("using undefined is silently skipped", () => {
    let reached = false;
    {
      using resource = undefined;
      reached = true;
    }
    expect(reached).toBe(true);
  });

  test("throws TypeError for non-null/non-undefined without Symbol.dispose", () => {
    expect(() => {
      using resource = { name: "not disposable" };
    }).toThrow(TypeError);
  });

  test("throws TypeError for primitives (number)", () => {
    expect(() => {
      using resource = 42;
    }).toThrow(TypeError);
  });

  test("throws TypeError when Symbol.dispose is not a function", () => {
    expect(() => {
      using resource = { [Symbol.dispose]: "not a function" };
    }).toThrow(TypeError);
  });

  test("disposes even when block throws", () => {
    let disposed = false;
    try {
      {
        using resource = {
          [Symbol.dispose]() { disposed = true; }
        };
        throw "block error";
      }
    } catch (e) {
      // expected
    }
    expect(disposed).toBe(true);
  });

  test("SuppressedError when both block and disposal throw", () => {
    let caught;
    try {
      {
        using resource = {
          [Symbol.dispose]() { throw "dispose error"; }
        };
        throw "block error";
      }
    } catch (e) {
      caught = e;
    }
    expect(caught instanceof SuppressedError).toBe(true);
    expect(caught.error).toBe("dispose error");
    expect(caught.suppressed).toBe("block error");
  });

  test("disposal error propagates when block completes normally", () => {
    let caught;
    try {
      {
        using resource = {
          [Symbol.dispose]() { throw "dispose error"; }
        };
      }
    } catch (e) {
      caught = e;
    }
    expect(caught).toBe("dispose error");
  });

  test("multiple disposal errors chain as SuppressedError", () => {
    let caught;
    try {
      {
        using a = {
          [Symbol.dispose]() { throw "error a"; }
        };
        using b = {
          [Symbol.dispose]() { throw "error b"; }
        };
      }
    } catch (e) {
      caught = e;
    }
    // b disposed first (reverse order), then a
    // error a suppresses error b -> SuppressedError(error=a, suppressed=b)
    expect(caught instanceof SuppressedError).toBe(true);
    expect(caught.error).toBe("error a");
    expect(caught.suppressed).toBe("error b");
  });

  test("multiple using in same statement", () => {
    const order = [];
    {
      using a = { [Symbol.dispose]() { order.push("a"); } },
            b = { [Symbol.dispose]() { order.push("b"); } };
    }
    expect(order).toEqual(["b", "a"]);
  });

  test("using in switch case disposes on break", () => {
    const order = [];

    switch (1) {
      case 1:
        using resource = {
          [Symbol.dispose]() { order.push("dispose"); }
        };
        order.push("body");
        break;
      default:
        order.push("default");
    }

    expect(order).toEqual(["body", "dispose"]);
  });

  test("using in switch case disposes on return", () => {
    const order = [];

    const run = () => {
      switch (1) {
        case 1:
          using resource = {
            [Symbol.dispose]() { order.push("dispose"); }
          };
          order.push("body");
          return "done";
      }
    };

    expect(run()).toBe("done");
    expect(order).toEqual(["body", "dispose"]);
  });

  test("using in switch case disposes when body throws", () => {
    const order = [];
    let caught;

    try {
      switch (1) {
        case 1:
          using resource = {
            [Symbol.dispose]() { order.push("dispose"); }
          };
          order.push("body");
          throw "boom";
      }
    } catch (e) {
      caught = e;
    }

    expect(caught).toBe("boom");
    expect(order).toEqual(["body", "dispose"]);
  });

  test("unmatched switch with using case does not leave active handler", () => {
    const order = [];
    let caught;

    try {
      switch (0) {
        case 1:
          using resource = {
            [Symbol.dispose]() { order.push("dispose"); }
          };
          order.push("body");
          break;
      }
      throw "after";
    } catch (e) {
      caught = e;
    }

    expect(caught).toBe("after");
    expect(order).toEqual([]);
  });
});
