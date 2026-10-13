/*---
description: >
  An async function suspended at an await survives a collection that runs
  while the await reads the awaited value's `then`.
features: [async-await, getters, Goccia.gc]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("a collection while await reads then", () => {
  test("resumes the async function with the awaited object", async () => {
    const thenable = {
      get then() {
        Goccia.gc();
        return undefined;
      },
    };
    const run = async (label) => {
      const local = { label };
      const value = await thenable;
      Goccia.gc();
      return `${local.label}:${value === thenable}`;
    };

    expect(await run("done")).toBe("done:true");
  });

  test("rejects into the async function when the getter throws", async () => {
    const thenable = {
      get then() {
        Goccia.gc();
        throw new TypeError("then getter failed");
      },
    };
    const run = async () => {
      const local = { caught: null };
      try {
        await thenable;
      } catch (error) {
        local.caught = error.message;
      }
      Goccia.gc();
      return local.caught;
    };

    expect(await run()).toBe("then getter failed");
  });

  test("resumes after a then method that collects before resolving", async () => {
    const thenable = {
      then(resolve) {
        Goccia.gc();
        resolve(7);
      },
    };
    const run = async () => {
      const local = { factor: 6 };
      const value = await thenable;
      Goccia.gc();
      return value * local.factor;
    };

    expect(await run()).toBe(42);
  });
});
