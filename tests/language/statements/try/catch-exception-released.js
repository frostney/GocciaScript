/*---
description: >
  The exception reaches a catch clause in a register that the clause's
  binding copies. Nothing reads that register again, so the clause clears it:
  an exception the clause has dropped its binding to, or never bound, is
  reclaimable by a collection in the clause (ES2026 §14.15.3 try statement).
features: [try-catch, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

describe("an exception object a catch clause no longer references", () => {
  test("is reclaimed when the clause drops its binding", () => {
    const run = () => {
      Goccia.gc();
      const baseline = Goccia.gc.bytesAllocated;
      let tag = null;
      try {
        throw { payload: new ArrayBuffer(SIZE), tag: "thrown" };
      } catch (error) {
        tag = error.tag;
        error = null;
        Goccia.gc();
      }
      return [tag, Goccia.gc.bytesAllocated - baseline];
    };
    const [tag, grown] = run();
    expect(tag).toBe("thrown");
    expect(grown).toBeLessThan(SIZE / 2);
  });

  test("is reclaimed when the clause has no binding", () => {
    const run = () => {
      Goccia.gc();
      const baseline = Goccia.gc.bytesAllocated;
      try {
        throw { payload: new ArrayBuffer(SIZE) };
      } catch {
        Goccia.gc();
      }
      return Goccia.gc.bytesAllocated - baseline;
    };
    expect(run()).toBeLessThan(SIZE / 2);
  });

  test("still rethrows from a clause that rethrows its binding", () => {
    const run = () => {
      try {
        try {
          throw new Error("inner");
        } catch (error) {
          Goccia.gc();
          throw error;
        }
      } catch (outer) {
        return outer.message;
      }
      return "not thrown";
    };
    expect(run()).toBe("inner");
  });

  test("keeps a destructured binding after the register is cleared", () => {
    const run = () => {
      try {
        throw { code: 7, detail: { text: "kept" } };
      } catch ({ code, detail: { text } }) {
        Goccia.gc();
        return [code, text];
      }
    };
    expect(run()).toEqual([7, "kept"]);
  });
});
