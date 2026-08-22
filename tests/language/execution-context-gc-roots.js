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
// the callee is left unreferenced by anything but its own frame, and each case
// collects twice so a survivor has to be genuinely rooted rather than merely
// not-yet-swept.
const collect = () => {
  Goccia.gc();
  Goccia.gc();
};

// Handing the callee to a helper (`callAndDrop(fn)`) is NOT enough to leave it
// unrooted: the helper's own parameter binding lives in an active-rooted call
// scope for the whole body, so the callee stays reachable through it and the
// function-value half of the invariant is never exercised. Every case below
// therefore invokes its callee straight off the expression that produced it —
// no binding anywhere — so the running frame really is the last thing
// referring to it.

describe("execution-context stack under collection", () => {
  test("unreferenced callee collects mid-body", () => {
    expect((() => { collect(); return 1; })()).toBe(1);
  });

  test("deep recursion collects at the bottom", () => {
    // A self-recursive function is necessarily bound to a name, so this case
    // cannot be made binding-free; it covers stack *depth* at collection time
    // (the entry-count half of the invariant), not the unrooted-callee half.
    const deep = (n) => (n === 0 ? (collect(), 0) : deep(n - 1) + 1);
    expect(deep(60)).toBe(60);
  });

  test("nested unreferenced callees collect at every level", () => {
    const build = (n) =>
      n === 0
        ? () => { collect(); return 0; }
        : () => { collect(); return build(n - 1)() + 1; };
    // build(20) is invoked straight off the call expression: the closure it
    // returns is never bound to anything.
    expect(build(20)()).toBe(20);
  });

  test("getter-triggered collection during an unreferenced call", () => {
    const holder = {};
    Object.defineProperty(holder, "x", {
      get() { collect(); return 7; },
      configurable: true,
    });
    expect((() => holder.x)()).toBe(7);
  });

  test("native re-entry (sort/map/reduce/reviver) with dropped callbacks", () => {
    expect([3, 1, 2].sort((a, b) => { collect(); return a - b; })).toEqual([1, 2, 3]);
    expect([1, 2].map((v) => { collect(); return v * 2; })).toEqual([2, 4]);
    expect([1, 2].reduce((a, v) => { collect(); return a + v; }, 0)).toBe(3);
    expect(JSON.parse('{"a":1}', (k, v) => { collect(); return v; })).toEqual({ a: 1 });
  });

  test("nested native re-entry with an unreferenced inner callee", () => {
    const out = [1, 2].map((v) =>
      [v].map(() => (() => { collect(); return v * 10; })())[0]
    );
    expect(out).toEqual([10, 20]);
  });

  test("Proxy traps re-enter and collect", () => {
    const p = new Proxy({}, {
      get(t, k) {
        if (k === "then") return undefined;
        return (() => { collect(); return String(k); })();
      },
    });
    expect(p.hello).toBe("hello");
  });

  test("coercion hooks re-enter and collect", () => {
    const box = {
      valueOf() { return (() => { collect(); return 5; })(); },
    };
    // Number(box) rather than +box: the direct operator forms are covered by
    // the VM operand rooting tests, and going through the wrapper keeps this
    // file orthogonal to that bug.
    expect(Number(box)).toBe(5);
    expect(String({ toString() { collect(); return "s"; } })).toBe("s");
    const sym = { [Symbol.toPrimitive]() { collect(); return 3; } };
    expect(Number(sym)).toBe(3);

    // The same hooks driven straight from the operators, whose operands are
    // materialized into native temporaries the collector cannot see unless the
    // opcode roots them. Exhaustive per-operator coverage lives in
    // tests/language/expressions/operand-gc-roots/.
    expect(box * 7).toBe(35);
    expect(box - 2).toBe(3);
    expect(box < 9).toBe(true);
    expect(sym * 7).toBe(21);
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
      thrower(15);
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
    // The generator function and the object holding it are never bound: the
    // iterator is the only thing that survives the expression.
    const it = ({
      *values() {
        collect();
        yield 1;
        collect();
        yield 2;
        collect();
      },
    }).values();
    collect();
    expect(it.next().value).toBe(1);
    collect();
    expect(it.next().value).toBe(2);
    collect();
    expect(it.next().done).toBe(true);
  });

  test("async functions collect across awaits", async () => {
    const p = (async () => {
      collect();
      await Promise.resolve();
      collect();
      await Promise.resolve();
      collect();
      return "done";
    })();
    collect();
    expect(await p).toBe("done");
    collect();
  });

  test("async generators collect across yields", async () => {
    const it = ({
      async *values() {
        collect();
        yield 1;
        await Promise.resolve();
        collect();
        yield 2;
      },
    }).values();
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
    const d = new Derived();
    collect();
    expect(d.field).toBe(1);
    expect(d.derivedField).toBe(2);
    expect(d.method()).toBe("base+derived");
    collect();
  });

  test("getters that collect during construction", () => {
    const Ctor = class {
      constructor(o) { this.v = o.probe; }
    };
    const src = {};
    Object.defineProperty(src, "probe", { get() { collect(); return 42; } });
    expect(new Ctor(src).v).toBe(42);
    collect();
  });

  test("bound dropped functions", () => {
    // Only the bound wrapper survives; the target method and the object
    // literal that carried it are never bound to anything.
    const bound = ({ probe() { collect(); return this.v; } })
      .probe.bind({ v: 9 });
    collect();
    expect(bound()).toBe(9);
    collect();
    expect(bound.call(null)).toBe(9);
  });

  test("tagged templates re-enter and collect", () => {
    const tag = (strings, ...vals) => { collect(); return strings.raw[0] + vals[0]; };
    expect(tag`a${1}b`).toBe("a1");
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
