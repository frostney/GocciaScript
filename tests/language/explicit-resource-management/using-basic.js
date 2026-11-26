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

  test("accepts contextual binding names", () => {
    const disposed = [];
    {
      using as = {
        name: "as",
        [Symbol.dispose]() { disposed.push(as.name); }
      };
      using from = {
        name: "from",
        [Symbol.dispose]() { disposed.push(from.name); }
      };
      using resource = {
        name: "static",
        [Symbol.dispose]() { disposed.push(resource.name); }
      };

      expect(as.name).toBe("as");
      expect(from.name).toBe("from");
      expect(resource.name).toBe("static");
    }
    expect(disposed).toEqual(["static", "from", "as"]);
  });

  test("for-of using accepts contextual binding names", () => {
    const disposed = [];
    const resources = [
      {
        name: "first",
        [Symbol.dispose]() { disposed.push(this.name); }
      },
      {
        name: "second",
        [Symbol.dispose]() { disposed.push(this.name); }
      }
    ];
    const seen = [];

    for (using as of resources) {
      seen.push(as.name);
    }

    expect(seen).toEqual(["first", "second"]);
    expect(disposed).toEqual(["first", "second"]);
  });

  test("for-of using disposers can close over the iteration binding", () => {
    const disposed = [];
    const resources = [
      {
        name: "first",
        [Symbol.dispose]() {
          if (typeof Goccia !== "undefined") {
            Goccia.gc();
          }
          disposed.push(this.readName());
        }
      }
    ];

    for (using as of resources) {
      expect(as.name).toBe("first");
      as.readName = () => as.name;
    }

    expect(disposed).toEqual(["first"]);
  });

  test("for-of using rejects non-disposable contextual binding values", () => {
    expect(() => {
      for (using as of [{ name: "plain" }]) {
        as.name;
      }
    }).toThrow(TypeError);
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

  // A using declaration directly in a case or default clause is a SyntaxError
  // (ES2027 draft §14.3.1.1; tests/language/modules/using-early-errors.js);
  // a block inside the clause holds one, disposed when the block ends.
  test("using in a block of a switch case disposes on break", () => {
    const order = [];

    switch (1) {
      case 1: {
        using resource = {
          [Symbol.dispose]() { order.push("dispose"); }
        };
        order.push("body");
        break;
      }
      default:
        order.push("default");
    }

    expect(order).toEqual(["body", "dispose"]);
  });

  test("using in a block of a switch case disposes on return", () => {
    const order = [];

    const run = () => {
      switch (1) {
        case 1: {
          using resource = {
            [Symbol.dispose]() { order.push("dispose"); }
          };
          order.push("body");
          return "done";
        }
      }
    };

    expect(run()).toBe("done");
    expect(order).toEqual(["body", "dispose"]);
  });

  test("using in a block of a switch case disposes when the body throws", () => {
    const order = [];
    let caught;

    try {
      switch (1) {
        case 1: {
          using resource = {
            [Symbol.dispose]() { order.push("dispose"); }
          };
          order.push("body");
          throw "boom";
        }
      }
    } catch (e) {
      caught = e;
    }

    expect(caught).toBe("boom");
    expect(order).toEqual(["body", "dispose"]);
  });

  test("an unmatched case's using block leaves no active handler", () => {
    const order = [];
    let caught;

    try {
      switch (0) {
        case 1: {
          using resource = {
            [Symbol.dispose]() { order.push("dispose"); }
          };
          order.push("body");
          break;
        }
      }
      throw "after";
    } catch (e) {
      caught = e;
    }

    expect(caught).toBe("after");
    expect(order).toEqual([]);
  });

  test("a skipped case's using block does not dispose the case-test value", () => {
    const order = [];
    const marker = () => {
      order.push("stale-dispose");
    };

    switch (marker) {
      // biome-ignore lint/suspicious/noFallthroughSwitchClause: intentional - the matching case after it must not dispose the skipped block's resource.
      case 1: {
        using skipped = {
          [Symbol.dispose]() { order.push("skipped"); }
        };
        order.push("case1");
      }
      case marker:
        order.push("case2");
        break;
    }

    expect(order).toEqual(["case2"]);
  });

  test("block-wrapped switch case using disposes before fallthrough", () => {
    const order = [];

    switch (1) {
      // biome-ignore lint/suspicious/noFallthroughSwitchClause: intentional - block disposal before fallthrough is the expected contrast.
      case 1: {
        using resource = {
          [Symbol.dispose]() { order.push("dispose"); }
        };
        order.push("case1");
      }
      case 2:
        order.push("case2");
        break;
    }

    expect(order).toEqual(["case1", "dispose", "case2"]);
  });

  test("a skipped case's using block stays inactive before a return", () => {
    const order = [];
    const marker = () => {
      order.push("stale-dispose");
    };

    const run = () => {
      switch (marker) {
        // biome-ignore lint/suspicious/noFallthroughSwitchClause: intentional - the later return must not dispose the skipped block's resource.
        case 1: {
          using skipped = {
            [Symbol.dispose]() { order.push("skipped"); }
          };
          order.push("case1");
        }
        case marker:
          order.push("case2");
          return "done";
      }
    };

    expect(run()).toBe("done");
    expect(order).toEqual(["case2"]);
  });

  test("a skipped case's using block stays inactive before a throw", () => {
    const order = [];
    const marker = () => {
      order.push("stale-dispose");
    };
    let caught;

    try {
      switch (marker) {
        // biome-ignore lint/suspicious/noFallthroughSwitchClause: intentional - the later throw must not dispose the skipped block's resource.
        case 1: {
          using skipped = {
            [Symbol.dispose]() { order.push("skipped"); }
          };
          order.push("case1");
        }
        case marker:
          order.push("case2");
          throw "boom";
      }
    } catch (e) {
      caught = e;
    }

    expect(caught).toBe("boom");
    expect(order).toEqual(["case2"]);
  });

  test("the default after a skipped case's using block", () => {
    const order = [];
    const marker = () => {
      order.push("stale-dispose");
    };

    switch (0) {
      // biome-ignore lint/suspicious/noFallthroughSwitchClause: intentional - default dispatch must not dispose the skipped block's resource.
      case marker: {
        using skipped = {
          [Symbol.dispose]() { order.push("skipped"); }
        };
        order.push("case1");
      }
      default:
        order.push("default");
        break;
    }

    expect(order).toEqual(["default"]);
  });
});
