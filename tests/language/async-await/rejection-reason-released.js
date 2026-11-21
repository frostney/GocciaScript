/*---
description: >
  An async function that catches the reason of an awaited rejection and drops
  its last reference to it lets a collection reclaim the reason in the same
  step: the job that resumed the function and the suspended frame's resume
  slot no longer keep it (ES2026 §27.7.5.3 Await).
features: [async-await, try-catch, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

describe("an awaited rejection's reason the catch clause has dropped", () => {
  test("is reclaimed by a collection in the clause", async () => {
    const reject = async () => {
      throw { payload: new ArrayBuffer(SIZE), tag: "reason" };
    };
    const run = async () => {
      Goccia.gc();
      const baseline = Goccia.gc.bytesAllocated;
      let tag = null;
      try {
        await reject();
      } catch (error) {
        tag = error.tag;
        error = null;
        Goccia.gc();
      }
      return [tag, Goccia.gc.bytesAllocated - baseline];
    };
    const [tag, grown] = await run();
    expect(tag).toBe("reason");
    expect(grown).toBeLessThan(SIZE / 2);
  });

  test("is still the thrown value while the clause holds it", async () => {
    const reject = async () => {
      throw new Error("held");
    };
    const run = async () => {
      try {
        await reject();
      } catch (error) {
        Goccia.gc();
        return error.message;
      }
      return "not thrown";
    };
    expect(await run()).toBe("held");
  });
});
