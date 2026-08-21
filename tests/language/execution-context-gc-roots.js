/*---
description: the execution-context stack survives collections from every re-entry shape
features: [Goccia.gc, generators, async-generators, async-await, Proxy, classes]
---*/

// Every entry the interpreter and the bytecode VM push onto the execution-
// context stack names a scope and (in the VM) a function value as raw
// pointers that no root source marks; the stack relies on those objects being
// reachable through the frame, closure or active-root that owns them for as
// long as the entry lives (see the rooting notes in
// Goccia.ExecutionContext.pas and Goccia.Realm.pas). These cases force
// collections at every point where the owning root could plausibly be gone:
// the callee is handed to a caller that keeps no reference, and each case
// collects twice so a survivor has to be genuinely rooted rather than merely
// not-yet-swept.
const collect = () => {
  Goccia.gc();
  Goccia.gc();
};

const callAndDrop = (fn) => fn();
const callAndDropWith = (fn, arg) => fn(arg);

describe("execution-context stack under collection", () => {
  test("dropped callee collects mid-body", () => {
    expect(callAndDrop(() => { collect(); return 1; })).toBe(1);
  });

  test("deep dropped-callee recursion collects at the bottom", () => {
    const deep = (n) => (n === 0 ? (collect(), 0) : deep(n - 1) + 1);
    expect(callAndDropWith(deep, 60)).toBe(60);
  });

  test("nested dropped callees collect at every level", () => {
    const build = (n) =>
      n === 0
        ? () => { collect(); return 0; }
        : () => { collect(); return build(n - 1)() + 1; };
    expect(callAndDrop(build(20))).toBe(20);
  });

  test("getter-triggered collection during a dropped call", () => {
    const holder = {};
    Object.defineProperty(holder, "x", {
      get() { collect(); return 7; },
      configurable: true,
    });
    expect(callAndDrop(() => holder.x)).toBe(7);
  });

  test("native re-entry (sort/map/reduce/reviver) with dropped callbacks", () => {
    expect([3, 1, 2].sort((a, b) => { collect(); return a - b; })).toEqual([1, 2, 3]);
    expect([1, 2].map((v) => { collect(); return v * 2; })).toEqual([2, 4]);
    expect([1, 2].reduce((a, v) => { collect(); return a + v; }, 0)).toBe(3);
    expect(JSON.parse('{"a":1}', (k, v) => { collect(); return v; })).toEqual({ a: 1 });
  });

  test("nested native re-entry with a dropped inner callee", () => {
    const out = [1, 2].map((v) =>
      [v].map(() => callAndDrop(() => { collect(); return v * 10; }))[0]
    );
    expect(out).toEqual([10, 20]);
  });

  test("Proxy traps re-enter and collect", () => {
    const p = new Proxy({}, {
      get(t, k) {
        if (k === "then") return undefined;
        return callAndDrop(() => { collect(); return String(k); });
      },
    });
    expect(p.hello).toBe("hello");
  });

  test("coercion hooks re-enter and collect", () => {
    const box = {
      valueOf() { return callAndDrop(() => { collect(); return 5; }); },
    };
    expect(Number(box)).toBe(5);
    expect(String({ toString() { collect(); return "s"; } })).toBe("s");
    const sym = { [Symbol.toPrimitive]() { collect(); return 3; } };
    expect(Number(sym)).toBe(3);
  });

  test("error-path unwinding leaves no stale entries", () => {
    const thrower = (n) => {
      if (n === 0) { collect(); throw new Error("deep"); }
      try {
        return thrower(n - 1);
      } finally {
        collect();
      }
    };
    let caught = null;
    try {
      callAndDropWith(thrower, 15);
    } catch (e) {
      caught = e;
    }
    collect();
    expect(caught.message).toBe("deep");
    collect();
  });

  test("throw across a native re-entry boundary", () => {
    let caught = null;
    try {
      [1, 2, 3].forEach((v) => {
        collect();
        if (v === 2) throw new Error("from callback");
      });
    } catch (e) {
      caught = e;
    }
    collect();
    expect(caught.message).toBe("from callback");
  });

  test("generators collect between resumptions", () => {
    const it = callAndDrop(() =>
      ({
        *values() {
          collect();
          yield 1;
          collect();
          yield 2;
          collect();
        },
      }).values());
    collect();
    expect(it.next().value).toBe(1);
    collect();
    expect(it.next().value).toBe(2);
    collect();
    expect(it.next().done).toBe(true);
  });

  test("async functions collect across awaits", async () => {
    const p = callAndDrop(async () => {
      collect();
      await Promise.resolve();
      collect();
      await Promise.resolve();
      collect();
      return "done";
    });
    collect();
    expect(await p).toBe("done");
    collect();
  });

  test("async generators collect across yields", async () => {
    const it = callAndDrop(() =>
      ({
        async *values() {
          collect();
          yield 1;
          await Promise.resolve();
          collect();
          yield 2;
        },
      }).values());
    collect();
    expect((await it.next()).value).toBe(1);
    collect();
    expect((await it.next()).value).toBe(2);
    collect();
  });

  test("class construction, fields and super collect", () => {
    const makeBase = () =>
      class {
        field = (collect(), 1);
        constructor() { collect(); }
        method() { collect(); return "base"; }
      };
    const Base = makeBase();
    const Derived = class extends Base {
      derivedField = (collect(), 2);
      constructor() { collect(); super(); collect(); }
      method() { collect(); return super.method() + "+derived"; }
    };
    const d = callAndDrop(() => new Derived());
    collect();
    expect(d.field).toBe(1);
    expect(d.derivedField).toBe(2);
    expect(d.method()).toBe("base+derived");
    collect();
  });

  test("getters that collect during construction", () => {
    const Ctor = callAndDrop(() => class {
      constructor(o) { this.v = o.probe; }
    });
    const src = {};
    Object.defineProperty(src, "probe", { get() { collect(); return 42; } });
    expect(new Ctor(src).v).toBe(42);
    collect();
  });

  test("bound dropped functions", () => {
    const bound = callAndDrop(() =>
      ({ probe() { collect(); return this.v; } }).probe.bind({ v: 9 }));
    collect();
    expect(bound()).toBe(9);
    collect();
    expect(bound.call(null)).toBe(9);
  });

  test("tagged templates re-enter and collect", () => {
    const tag = (strings, ...vals) => { collect(); return strings.raw[0] + vals[0]; };
    expect(callAndDrop(() => tag`a${1}b`)).toBe("a1");
    collect();
  });

  test("microtask drain with dropped resolvers", async () => {
    const results = [];
    await new Promise((resolve) => {
      collect();
      Promise.resolve().then(() => {
        collect();
        results.push("micro");
        resolve();
      });
    });
    collect();
    expect(results).toEqual(["micro"]);
  });
});

collect();
