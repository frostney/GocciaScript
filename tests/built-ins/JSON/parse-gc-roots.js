/*---
description: JSON.parse keeps its parse records, its materialised subtrees and its reviver results reachable across collecting guest code
features: [Goccia.gc, json-parse-with-source]
---*/

const hasGoccia = typeof Goccia !== "undefined";

// A bare gc() usually leaves a freed slot readable; the allocation churn after
// it is what makes a collected temporary observable.
const churn = () => {
  Goccia.gc();
  let total = 0;
  for (const i of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
    const scratch = { a: i * 7.5, b: [i, i + 1], c: "x" + i };
    total += scratch.a + scratch.b[0];
  }
  return total;
};

// Revivers need call-site `this` (the holder), so they are method shorthands
// pulled off a throwaway object rather than arrows.
describe.runIf(hasGoccia)("JSON.parse GC roots", () => {
  test("a parse record outlives the value the reviver replaced", () => {
    // The parse-record tree keeps a pointer to the value the *text* produced
    // so the walk can decide whether `context.source` still applies. A reviver
    // that overwrites a sibling before the walk reaches it makes the record
    // the last reference to the original value; the walk reads that pointer
    // back afterwards.
    const reviver = {
      revive(key, value) {
        if (key === "a") {
          this.b = 5;
          churn();
        }
        return value;
      },
    }.revive;

    const parsed = JSON.parse('{"a":1,"b":98765.4321}', reviver);
    expect(parsed.b).toBe(5);
  });

  test("a large parse-record string outlives the reviver that replaced it", () => {
    // Same window as above, sized so the freed payload is a block the
    // allocator is likely to hand back rather than one that reads back intact.
    //
    // READ THIS BEFORE TRUSTING A PASS. These tests document the shape; they
    // do not prove the window is closed. The hazard is established by
    // inspection — ApplyReviver reaches IsSameValue -> ValuesEqual, which
    // dispatches on a value the parse record is the last holder of once a
    // reviver has overwritten the property it came from. Whether reading that
    // freed object *faults* is an allocator question, and the answer is not
    // portable: with the rooting removed this shape aborts GocciaScriptLoader
    // with an access violation on arm64 macOS/FPC 3.2.2, does not fault under
    // the parallel test runner (per-worker heaps keep the block readable), and
    // does not fault at all on some other platforms. Attempts to turn it into
    // a deterministic wrong-value assertion by forcing the freed slot to be
    // recycled into an equal value did not reproduce either. So: a pass here
    // is not verification, and a failure is a real regression.
    const big = "x".repeat(1000000);
    let filler = "";
    const reviver = {
      revive(key, value) {
        if (key === "a") {
          this.b = "small";
          Goccia.gc();
          // Reclaim-sized churn: a block this large is handed back to the
          // allocator, and only a comparable allocation reuses it.
          filler = "y".repeat(1000000) + "z";
        }
        return value;
      },
    }.revive;

    const parsed = JSON.parse('{"a":1,"b":"' + big + '"}', reviver);
    expect(parsed.b).toBe("small");
    expect(filler.length).toBe(1000001);
  });

  test("a parse record outlives a replaced sibling further along", () => {
    const reviver = {
      revive(key, value) {
        if (key === "a") {
          this.b = 4242.75;
          churn();
        }
        return value;
      },
    }.revive;

    const parsed = JSON.parse('{"a":1,"b":"replaced-away","c":3}', reviver);
    expect(parsed.b).toBe(4242.75);
    expect(parsed.c).toBe(3);
  });

  test("a nested parse record survives a collecting reviver", () => {
    const reviver = {
      revive(key, value) {
        if (key === "x") {
          churn();
        }
        return value;
      },
    }.revive;

    const parsed = JSON.parse('{"o":{"x":11.5,"y":22.5},"z":33.5}', reviver);
    expect(parsed.o.y).toBe(22.5);
    expect(parsed.z).toBe(33.5);
  });

  test("context.source still reads after a collecting reviver", () => {
    const seen = [];
    const reviver = {
      revive(key, value, context) {
        if (key === "a") {
          churn();
        }
        if (context && typeof context.source === "string") {
          seen.push(key + "=" + context.source);
        }
        return value;
      },
    }.revive;

    JSON.parse('{"a":1.25,"b":2.5}', reviver);
    expect(seen).toEqual(["a=1.25", "b=2.5"]);
  });

  test("a reviver result survives the store that follows it", () => {
    // Canary for the window between the reviver returning a fresh object and
    // the holder storing it: the recursion has released its own roots by then.
    const reviver = {
      revive(key, value) {
        if (key === "") {
          return value;
        }
        churn();
        return { tag: key };
      },
    }.revive;

    const parsed = JSON.parse('{"a":1,"b":2}', reviver);
    expect(parsed.a.tag).toBe("a");
    expect(parsed.b.tag).toBe("b");
  });

  test("a materialised subtree survives being handed to its parent", () => {
    // Canary for the container hand-off: OnEndObject/OnEndArray pop a finished
    // subtree off the visitor's open-container stack before storing it into
    // its parent, so for the width of that store it is held by one native
    // local. Wide enough that the parent's storage has to grow at the store.
    const text =
      '{"k0":{"n":0},"k1":{"n":1},"k2":{"n":2},"k3":{"n":3},"k4":{"n":4},' +
      '"k5":{"n":5},"k6":{"n":6},"k7":{"n":7},"k8":{"n":8},"k9":{"n":9},' +
      '"a":[[0],[1],[2],[3],[4],[5],[6],[7],[8],[9]]}';
    const parsed = JSON.parse(text);
    churn();

    for (const i of [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) {
      expect(parsed["k" + i].n).toBe(i);
      expect(parsed.a[i][0]).toBe(i);
    }
  });
});
