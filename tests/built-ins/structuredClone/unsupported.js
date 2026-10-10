/*---
description: structuredClone throws DataCloneError for objects it has no serialization for
features: [structuredClone]
---*/

// StructuredSerializeInternal throws for an object with an internal slot it
// has no branch for and for a platform object that is not serializable,
// instead of copying its properties into a plain object. A Proxy throws too,
// even around an array, which the spec's IsArray step would see through: this
// follows V8 and SpiderMonkey.
const expectDataCloneError = (value) => {
  let error;
  try {
    structuredClone(value);
  } catch (e) {
    error = e;
  }
  expect(error instanceof DOMException).toBe(true);
  expect(error.name).toBe("DataCloneError");
};

describe("values with no serialization", () => {
  test("a Proxy", () => {
    expectDataCloneError(new Proxy({}, {}));
    expectDataCloneError(new Proxy([], {}));
  });

  test("a Symbol object", () => {
    expectDataCloneError(Object(Symbol("s")));
  });

  test("a Promise", () => {
    expectDataCloneError(Promise.resolve(1));
  });

  test("iterators and generators", () => {
    expectDataCloneError([1].values());
    expectDataCloneError(new Map().entries());
    expectDataCloneError("a".matchAll(/a/g));
    expectDataCloneError([1].values().map((x) => x));
    const generator = { *items() {} }.items();
    expectDataCloneError(generator);
  });

  test("a DisposableStack", () => {
    expectDataCloneError(new DisposableStack());
  });

  test("a Temporal object", () => {
    expectDataCloneError(Temporal.Duration.from({ days: 1 }));
  });

  test("platform objects that are not serializable", () => {
    expectDataCloneError(new URL("http://example.com/"));
    expectDataCloneError(new Headers());
  });

  test("inside another value", () => {
    expectDataCloneError({ nested: Promise.resolve() });
    expectDataCloneError([new Proxy({}, {})]);
  });
});

describe("values that still clone through the property walk", () => {
  test("a class instance and a null-prototype object", () => {
    class Point {
      constructor() {
        this.x = 1;
      }
    }
    expect(structuredClone(new Point())).toEqual({ x: 1 });
    const bare = Object.create(null);
    bare.y = 2;
    expect(structuredClone(bare).y).toBe(2);
  });

  // A DisposableStack is recognized by its class, not by looking its address
  // up in the stacks' side table, which still lists stacks that have been
  // collected.
  test("a plain object allocated after DisposableStacks were collected", () => {
    const makeStacks = () => Array.from({ length: 50 }, () => new DisposableStack()).length;
    makeStacks();
    Goccia.gc();
    const objects = Array.from({ length: 100 }, (_, i) => {
      const bare = Object.create(null);
      bare.i = i;
      return bare;
    });
    expect(structuredClone(objects).map((o) => o.i)).toEqual(objects.map((o) => o.i));
  });

  test("the object Proxy.revocable returns, once its functions are gone", () => {
    const revocable = Proxy.revocable({}, {});
    delete revocable.proxy;
    delete revocable.revoke;
    revocable.x = 1;
    expect(structuredClone(revocable)).toEqual({ x: 1 });
  });

  test("namespace objects without internal slots", () => {
    expect(structuredClone(Math)).toEqual({});
    expect(structuredClone(JSON)).toEqual({});
  });
});
