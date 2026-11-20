/*---
description: A generator's return() at a yield after a using declaration disposes the resources of every block it leaves (ES2027 §27.8.3.4 GeneratorResumeAbrupt, §7.5.4 DisposeResources)
features: [explicit-resource-management, generators, async-generators]
---*/

const resource = (name, log) => ({
  [Symbol.dispose]() {
    log.push("dispose " + name);
  },
});

const asyncResource = (name, log) => ({
  async [Symbol.asyncDispose]() {
    await null;
    log.push("dispose " + name);
  },
});

const throwingResource = (name, log) => ({
  [Symbol.dispose]() {
    log.push("dispose " + name);
    throw new Error(name);
  },
});

describe("generator return() through a using declaration", () => {
  test("disposes a using declaration of the generator body", () => {
    const log = [];
    function* g() {
      using r = resource("r", log);
      yield 1;
      log.push("unreachable");
    }
    const it = g();
    it.next();
    expect(it.return(9)).toEqual({ value: 9, done: true });
    expect(log).toEqual(["dispose r"]);
    expect(it.next()).toEqual({ value: undefined, done: true });
  });

  test("disposes the resources of every block it leaves, innermost first", () => {
    const log = [];
    function* g() {
      using a = resource("a", log);
      {
        using b = resource("b", log);
        using c = resource("c", log);
        yield 1;
      }
    }
    const it = g();
    it.next();
    expect(it.return(9)).toEqual({ value: 9, done: true });
    expect(log).toEqual(["dispose c", "dispose b", "dispose a"]);
  });

  test("skips a using declaration the block has not reached", () => {
    const log = [];
    function* g() {
      using a = resource("a", log);
      yield 1;
      using b = resource("b", log);
    }
    const it = g();
    it.next();
    it.return(9);
    expect(log).toEqual(["dispose a"]);
  });

  test("a dispose error replaces the return completion", () => {
    const log = [];
    function* g() {
      {
        using r = throwingResource("r", log);
        yield 1;
      }
    }
    const it = g();
    it.next();
    let caught;
    try {
      it.return(9);
    } catch (e) {
      caught = e;
    }
    expect(caught instanceof SuppressedError).toBe(false);
    expect(caught.message).toBe("r");
    expect(log).toEqual(["dispose r"]);
    expect(it.next()).toEqual({ value: undefined, done: true });
  });

  test("a second dispose error suppresses the first", () => {
    const log = [];
    function* g() {
      using a = throwingResource("a", log);
      using b = throwingResource("b", log);
      yield 1;
    }
    const it = g();
    it.next();
    let caught;
    try {
      it.return(9);
    } catch (e) {
      caught = e;
    }
    expect(caught instanceof SuppressedError).toBe(true);
    expect(caught.error.message).toBe("a");
    expect(caught.suppressed.message).toBe("b");
    expect(log).toEqual(["dispose b", "dispose a"]);
  });

  test("a finally around the block runs after the disposal", () => {
    const log = [];
    function* g() {
      try {
        using r = resource("r", log);
        yield 1;
      } finally {
        log.push("finally");
      }
    }
    const it = g();
    it.next();
    expect(it.return(9)).toEqual({ value: 9, done: true });
    expect(log).toEqual(["dispose r", "finally"]);
  });

  test("a catch around the block sees a dispose error, not the return", () => {
    const log = [];
    function* g() {
      try {
        using r = throwingResource("r", log);
        yield 1;
      } catch (e) {
        log.push("caught " + e.message);
      }
      return "after";
    }
    const it = g();
    it.next();
    expect(it.return(9)).toEqual({ value: "after", done: true });
    expect(log).toEqual(["dispose r", "caught r"]);
  });

  test("a using declaration in a for...of body disposes before the iterator closes", () => {
    const log = [];
    const source = {
      *values() {
        try {
          yield 1;
        } finally {
          log.push("close");
        }
      },
    };
    function* g() {
      for (const x of source.values()) {
        using r = resource("r", log);
        yield x;
      }
    }
    const it = g();
    it.next();
    expect(it.return(9)).toEqual({ value: 9, done: true });
    expect(log).toEqual(["dispose r", "close"]);
  });

  test("a dispose error in a for...of body wins over the iterator's close error", () => {
    const log = [];
    const source = {
      *values() {
        try {
          yield 1;
        } finally {
          log.push("close");
          throw new Error("close");
        }
      },
    };
    function* g() {
      for (const x of source.values()) {
        using r = throwingResource("r", log);
        yield x;
      }
    }
    const it = g();
    it.next();
    let caught;
    try {
      it.return(9);
    } catch (e) {
      caught = e;
    }
    expect(caught.message).toBe("r");
    expect(log).toEqual(["dispose r", "close"]);
  });

  test("a dispose error in a for (using x of ...) head replaces the return completion", () => {
    const log = [];
    function* g() {
      for (using r of [throwingResource("r", log)]) {
        yield 1;
      }
    }
    const it = g();
    it.next();
    let caught;
    try {
      it.return(9);
    } catch (e) {
      caught = e;
    }
    expect(caught instanceof SuppressedError).toBe(false);
    expect(caught.message).toBe("r");
    expect(log).toEqual(["dispose r"]);
  });

  test("a throw() still disposes and suppresses with the thrown error", () => {
    const log = [];
    function* g() {
      using r = throwingResource("r", log);
      yield 1;
    }
    const it = g();
    it.next();
    let caught;
    try {
      it.throw(new Error("thrown"));
    } catch (e) {
      caught = e;
    }
    expect(caught instanceof SuppressedError).toBe(true);
    expect(caught.error.message).toBe("r");
    expect(caught.suppressed.message).toBe("thrown");
  });
});

describe("async generator return() through a using declaration", () => {
  test("disposes a using declaration", async () => {
    const log = [];
    async function* g() {
      using r = resource("r", log);
      yield 1;
    }
    const it = g();
    await it.next();
    expect(await it.return(9)).toEqual({ value: 9, done: true });
    expect(log).toEqual(["dispose r"]);
  });

  test("awaits an await using declaration's disposal before it settles", async () => {
    const log = [];
    async function* g() {
      await using a = asyncResource("a", log);
      {
        await using b = asyncResource("b", log);
        yield 1;
      }
    }
    const it = g();
    await it.next();
    expect(await it.return(9)).toEqual({ value: 9, done: true });
    expect(log).toEqual(["dispose b", "dispose a"]);
  });

  test("rejects with a dispose error", async () => {
    const log = [];
    async function* g() {
      await using r = {
        async [Symbol.asyncDispose]() {
          log.push("dispose r");
          throw new Error("r");
        },
      };
      yield 1;
    }
    const it = g();
    await it.next();
    let caught;
    try {
      await it.return(9);
    } catch (e) {
      caught = e;
    }
    expect(caught instanceof SuppressedError).toBe(false);
    expect(caught.message).toBe("r");
    expect(log).toEqual(["dispose r"]);
    expect(await it.next()).toEqual({ value: undefined, done: true });
  });
});
