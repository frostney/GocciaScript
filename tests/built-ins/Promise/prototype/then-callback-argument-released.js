/*---
description: >
  A promise reaction job calls its handler with the settled value, and keeps
  that value rooted until the job ends. A handler that drops its parameter
  lets a collection inside it reclaim the value: the job lets go of its root
  once the handler's own frame holds the argument (ES2026 §27.2.2.1
  NewPromiseReactionJob).
features: [Promise, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

describe("Promise.prototype.then handler's dropped parameter", () => {
  test("is reclaimed by a collection inside the handler", async () => {
    Goccia.gc();
    const baseline = Goccia.gc.bytesAllocated;
    // The chain is built in a helper so that the awaiting frame holds neither
    // the promise nor its result.
    const chain = () =>
      Promise.resolve(new ArrayBuffer(SIZE)).then((buffer) => {
        const length = buffer.byteLength;
        buffer = null;
        Goccia.gc();
        return [length, Goccia.gc.bytesAllocated - baseline];
      });
    const grown = await chain();
    expect(grown[0]).toBe(SIZE);
    expect(grown[1]).toBeLessThan(SIZE / 2);
  });

  test("is reclaimed for a rejection handler's reason", async () => {
    Goccia.gc();
    const baseline = Goccia.gc.bytesAllocated;
    const chain = () =>
      Promise.reject({ payload: new ArrayBuffer(SIZE), tag: "reason" }).catch(
        (reason) => {
          const tag = reason.tag;
          reason = null;
          Goccia.gc();
          return [tag, Goccia.gc.bytesAllocated - baseline];
        },
      );
    const result = await chain();
    expect(result[0]).toBe("reason");
    expect(result[1]).toBeLessThan(SIZE / 2);
  });

  test("still passes the value to a handler that keeps it", async () => {
    const buffer = new ArrayBuffer(1024);
    const seen = await Promise.resolve(buffer).then((value) => {
      Goccia.gc();
      return value;
    });
    expect(seen).toBe(buffer);
  });

  test("passes a value a native handler uses after a nested collection", async () => {
    const result = await Promise.resolve([1, 2, 3]).then(Array.from).then((items) => {
      Goccia.gc();
      return items.length;
    });
    expect(result).toBe(3);
  });
});
