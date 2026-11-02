/*---
description: >
  A generator resumed by guest code while a promise reaction job is running
  must not let go of that job's argument: the job's native code still holds the
  rejection reason of a for await over a sync iterator after the iterator's
  return() has run guest code, and then throws it (ES2026 §27.1.4.2.1
  AsyncFromSyncIteratorContinuation closeOnRejection).
features: [async-iteration, generators, for-await-of, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

describe("a job argument that guest code also passes to a generator", () => {
  test("is still the rejection reason after return() collects", async () => {
    const holder = {
      *idle() {
        yield 0;
      },
    };
    const generator = holder.idle();
    generator.next();

    let reason = null;
    const iterable = {
      [Symbol.iterator]() {
        return {
          next() {
            reason = new ArrayBuffer(SIZE);
            return { value: Promise.reject(reason), done: false };
          },
          return() {
            // Resumes the generator with the same object the running job
            // holds as its argument, drops the program's references to it and
            // collects: the job must still hold the object afterwards.
            generator.next(reason);
            reason = null;
            Goccia.gc();
            return {};
          },
        };
      },
    };

    let caught = null;
    try {
      for await (const _ of iterable) {
        // Never reached: the first value is a rejection.
      }
    } catch (error) {
      caught = error;
    }
    expect(caught instanceof ArrayBuffer).toBe(true);
    expect(caught.byteLength).toBe(SIZE);
  });
});
