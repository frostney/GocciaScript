/*---
description: >
  A statement that awaits a container (Promise.all's array, an object) and
  destructures it leaves the container in a register above the live ones, and
  the frame outlives the statement. The statement clears those registers once
  it has resumed, so a value the function takes out of the container and then
  drops is reclaimable before the function next suspends (ES2026 §27.7.5.3
  Await).
features: [async-await, destructuring, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};

describe("a value taken out of an awaited container and dropped", () => {
  test("is reclaimed when the container is Promise.all's array", async () => {
    const make = async () => new ArrayBuffer(SIZE);
    const run = async () => {
      const baseline = collect();
      let [buffer] = await Promise.all([make()]);
      const length = buffer.byteLength;
      buffer = null;
      Goccia.gc();
      return [length, Goccia.gc.bytesAllocated - baseline];
    };
    const [length, grown] = await run();
    expect(length).toBe(SIZE);
    expect(grown).toBeLessThan(SIZE / 2);
  });

  test("is reclaimed when the container is an object", async () => {
    const make = async () => ({ buffer: new ArrayBuffer(SIZE), tag: "kept" });
    const run = async () => {
      const baseline = collect();
      let { buffer, tag } = await make();
      buffer = null;
      Goccia.gc();
      return [tag, Goccia.gc.bytesAllocated - baseline];
    };
    const [tag, grown] = await run();
    expect(tag).toBe("kept");
    expect(grown).toBeLessThan(SIZE / 2);
  });

  test("is reclaimed when the await sits in a call argument", async () => {
    const make = async () => new ArrayBuffer(SIZE);
    const measure = (buffer) => buffer.byteLength;
    const run = async () => {
      const baseline = collect();
      const length = measure(await make());
      Goccia.gc();
      return [length, Goccia.gc.bytesAllocated - baseline];
    };
    const [length, grown] = await run();
    expect(length).toBe(SIZE);
    expect(grown).toBeLessThan(SIZE / 2);
  });

  test("leaves the values the function still holds intact", async () => {
    const make = async (fill) => new Uint8Array(1024).fill(fill);
    const run = async () => {
      const [first, second] = await Promise.all([make(1), make(2)]);
      Goccia.gc();
      const third = await make(3);
      Goccia.gc();
      return [first[0], second[1023], third[512], first.length];
    };
    expect(await run()).toEqual([1, 2, 3, 1024]);
  });
});
