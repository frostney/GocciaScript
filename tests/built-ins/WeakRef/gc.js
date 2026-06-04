/*---
description: WeakRef integrates with Goccia.gc and kept objects
features: [WeakRef, Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("WeakRef GC behavior", () => {
  test("deref keeps the target alive for the current job", () => {
    let target = { marker: "kept" };
    const ref = new WeakRef(target);
    target = null;

    const first = ref.deref();
    Goccia.gc();
    expect(ref.deref()).toBe(first);
    expect(first.marker).toBe("kept");
  });

  test("target clears after it is unreachable across a job checkpoint", () => {
    let target = { marker: "collected" };
    const ref = new WeakRef(target);
    target = null;

    const result = new Promise((resolve) => {
      queueMicrotask(() => {});
      queueMicrotask(() => {
        Goccia.gc();
      });
      queueMicrotask(() => {
        Goccia.gc();
        resolve(ref.deref());
      });
    });

    return result.then((value) => {
      expect(value).toBe(undefined);
    });
  });

  test("live target survives explicit GC", () => {
    const target = { marker: "live" };
    const ref = new WeakRef(target);
    return Promise.resolve().then(() => {
      Goccia.gc();
      expect(ref.deref()).toBe(target);
      expect(ref.deref().marker).toBe("live");
    });
  });

  test("caught call argument errors do not keep argument roots alive", () => {
    let target = { marker: "call-root" };
    const ref = new WeakRef(target);
    const callee = () => undefined;
    const thrower = () => {
      throw new Error("boom");
    };

    try {
      callee(target, thrower());
    } catch (e) {}

    target = null;

    const result = new Promise((resolve) => {
      queueMicrotask(() => {});
      queueMicrotask(() => {
        Goccia.gc();
      });
      queueMicrotask(() => {
        Goccia.gc();
        resolve(ref.deref());
      });
    });

    return result.then((value) => {
      expect(value).toBe(undefined);
    });
  });

  test("caught binary coercion errors do not keep operand roots alive", () => {
    let target = { marker: "binary-root" };
    const ref = new WeakRef(target);
    const thrower = {
      [Symbol.toPrimitive]: () => {
        throw new Error("boom");
      },
    };

    try {
      target + thrower;
    } catch (e) {}

    target = null;

    const result = new Promise((resolve) => {
      queueMicrotask(() => {});
      queueMicrotask(() => {
        Goccia.gc();
      });
      queueMicrotask(() => {
        Goccia.gc();
        resolve(ref.deref());
      });
    });

    return result.then((value) => {
      expect(value).toBe(undefined);
    });
  });

  test("caught constructor argument errors do not keep argument roots alive", () => {
    let target = { marker: "new-root" };
    const ref = new WeakRef(target);
    const thrower = () => {
      throw new Error("boom");
    };
    class Holder {}

    try {
      new Holder(target, thrower());
    } catch (e) {}

    target = null;

    const result = new Promise((resolve) => {
      queueMicrotask(() => {});
      queueMicrotask(() => {
        Goccia.gc();
      });
      queueMicrotask(() => {
        Goccia.gc();
        resolve(ref.deref());
      });
    });

    return result.then((value) => {
      expect(value).toBe(undefined);
    });
  });
});
