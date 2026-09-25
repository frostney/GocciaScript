/*---
description: structuredClone deep-clones Map and Set
features: [structuredClone, Map, Set]
---*/

describe("Map cloning", () => {
  test("clones a Map", () => {
    const original = new Map();
    original.set("a", 1);
    original.set("b", 2);
    const clone = structuredClone(original);
    expect(clone.get("a")).toBe(1);
    expect(clone.get("b")).toBe(2);
    expect(clone.size).toBe(2);
  });

  test("clone is a distinct Map", () => {
    const original = new Map();
    original.set("key", "value");
    const clone = structuredClone(original);
    clone.set("key", "changed");
    expect(original.get("key")).toBe("value");
    expect(clone.get("key")).toBe("changed");
  });

  test("clones Map with object values", () => {
    const obj = { nested: true };
    const original = new Map();
    original.set("obj", obj);
    const clone = structuredClone(original);
    const clonedObj = clone.get("obj");
    expect(clonedObj.nested).toBe(true);
    clonedObj.nested = false;
    expect(obj.nested).toBe(true);
  });
});

describe("Set cloning", () => {
  test("clones a Set", () => {
    const original = new Set([1, 2, 3]);
    const clone = structuredClone(original);
    expect(clone.size).toBe(3);
    expect(clone.has(1)).toBe(true);
    expect(clone.has(2)).toBe(true);
    expect(clone.has(3)).toBe(true);
  });

  test("clone is a distinct Set", () => {
    const original = new Set([1, 2]);
    const clone = structuredClone(original);
    clone.add(3);
    expect(original.size).toBe(2);
    expect(clone.size).toBe(3);
  });

  test("clones Set with object values", () => {
    const obj = { value: 42 };
    const original = new Set([obj]);
    const clone = structuredClone(original);
    expect(clone.size).toBe(1);
    const [clonedObj] = [...clone];
    expect(clonedObj.value).toBe(42);
    clonedObj.value = 99;
    expect(obj.value).toBe(42);
  });
});

describe("Weak collection cloning", () => {
  test("WeakMap is not cloneable", () => {
    try {
      structuredClone(new WeakMap());
    } catch (e) {
      expect(e instanceof DOMException).toBe(true);
      expect(e.name).toBe("DataCloneError");
      expect(e.code).toBe(25);
      return;
    }
    expect(true).toBe(false);
  });

  test("WeakSet is not cloneable", () => {
    try {
      structuredClone(new WeakSet());
    } catch (e) {
      expect(e instanceof DOMException).toBe(true);
      expect(e.name).toBe("DataCloneError");
      expect(e.code).toBe(25);
      return;
    }
    expect(true).toBe(false);
  });

  test("WeakRef is not cloneable", () => {
    try {
      structuredClone(new WeakRef({}));
    } catch (e) {
      expect(e instanceof DOMException).toBe(true);
      expect(e.name).toBe("DataCloneError");
      expect(e.code).toBe(25);
      return;
    }
    expect(true).toBe(false);
  });

  test("FinalizationRegistry is not cloneable", () => {
    try {
      structuredClone(new FinalizationRegistry(() => {}));
    } catch (e) {
      expect(e instanceof DOMException).toBe(true);
      expect(e.name).toBe("DataCloneError");
      expect(e.code).toBe(25);
      return;
    }
    expect(true).toBe(false);
  });
});

// Same exposure as the object walk: see structuredClone/objects.js.
describe.runIf(typeof Goccia !== "undefined")("collection clones under explicit GC", () => {
  // A bare gc() usually leaves the freed slot readable; the allocation churn
  // afterwards is what makes a collected value observable.
  const gcChurn = () => {
    Goccia.gc();
    let total = 0;
    for (const i of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
      const scratch = { a: i * 7.5, b: [i, i + 1], c: "x" + i };
      total += scratch.a + scratch.b[0];
    }
    return total;
  };

  test("keeps a Map clone alive when an entry's getter collects", () => {
    const source = new Map();
    source.set("a", {
      get x() {
        gcChurn();
        return 1;
      },
      y: "z",
    });
    source.set("b", 2);

    const clone = structuredClone(source);
    expect(clone.size).toBe(2);
    expect(clone.get("a").x).toBe(1);
    expect(clone.get("b")).toBe(2);
  });

  test("keeps a Set clone alive when a member's getter collects", () => {
    const source = new Set();
    source.add({
      get x() {
        gcChurn();
        return 1;
      },
      y: "z",
    });
    source.add(2);

    const clone = structuredClone(source);
    expect(clone.size).toBe(2);
  });
});
