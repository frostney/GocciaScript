/*---
description: traditional for-loop lexical bindings survive explicit Goccia.gc calls in the body
features: [compat-traditional-for-loop, Goccia.gc]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("traditional for explicit GC", () => {
  const measureLoopRetainedGrowth = (iterations) => {
    Goccia.gc();
    const baseline = Goccia.gc.bytesAllocated;
    let retainedGrowth = 0;

    for (let i = 0; i < iterations; i++) {
      if (i === iterations - 1) {
        Goccia.gc();
        retainedGrowth = Goccia.gc.bytesAllocated - baseline;
      }
    }

    return retainedGrowth;
  };

  const measureGeneratorLoopRetainedGrowth = (iterations) => {
    const obj = {
      *measure() {
        Goccia.gc();
        const baseline = Goccia.gc.bytesAllocated;
        let retainedGrowth = 0;

        for (let i = 0; i < iterations; i++) {
          if (i === iterations - 1) {
            Goccia.gc();
            retainedGrowth = Goccia.gc.bytesAllocated - baseline;
          }
        }

        return retainedGrowth;
      },
    };

    return obj.measure().next().value;
  };

  test("preserves the current lexical iteration binding across Goccia.gc", () => {
    const values = [];

    for (let i = 0; i < 3; i++) {
      Goccia.gc();
      values.push(i);
    }

    expect(values).toEqual([0, 1, 2]);
  });

  test("keeps retained growth bounded across ordinary lexical iterations", () => {
    const shortGrowth = measureLoopRetainedGrowth(500);
    const longGrowth = measureLoopRetainedGrowth(5000);

    expect(Math.abs(longGrowth)).toBeLessThanOrEqual(
      Math.max(Math.abs(shortGrowth) * 3, 1),
    );
  });

  test("keeps retained growth bounded when a generator runs the loop in one resume", () => {
    const shortGrowth = measureGeneratorLoopRetainedGrowth(500);
    const longGrowth = measureGeneratorLoopRetainedGrowth(5000);

    expect(Math.abs(longGrowth)).toBeLessThanOrEqual(
      Math.max(Math.abs(shortGrowth) * 3, 1),
    );
  });
});
