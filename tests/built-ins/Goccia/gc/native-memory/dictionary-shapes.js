/*---
description: >
  The shapes of objects past 64 properties, and of proxies, count against the
  memory limit and are freed with their object. Making and dropping thousands
  of them under a 32 MiB limit, far more than the limit if none were freed,
  completes without a refusal.
features: [Goccia.gc]
---*/

const KEYS = Array.from({ length: 200 }, (_, i) => `key${i}`);

const wide = () => {
  const o = {};
  for (const k of KEYS) o[k] = 1;
  return o;
};

describe("dictionary shapes under the memory limit", () => {
  test("of wide objects are freed when the objects are dropped", () => {
    // about 9 KB of shape for each object: 45 MB for all of them
    let last = null;
    for (const _ of Array.from({ length: 5000 })) last = wide();
    expect(Object.keys(last).length).toBe(200);
  });

  test("a wide object's earlier shapes are freed when its attributes change", () => {
    // freezing changes each property's attributes, one new shape each time
    let last = null;
    for (const _ of Array.from({ length: 100 })) last = Object.freeze(wide());
    expect(Object.isFrozen(last)).toBe(true);
  });

  test("of proxies are freed when the proxies are dropped", () => {
    let last = null;
    for (const _ of Array.from({ length: 200000 })) last = new Proxy({}, {});
    expect(typeof last).toBe("object");
  });
});
