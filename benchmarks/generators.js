/*---
description: Generator benchmarks
---*/

suite("generators", () => {
  const values = Array.from({ length: 100 }, (_, i) => i);

  const makeObjectGenerator = () => ({
    *values() {
      yield* values;
    },
  });

  class GeneratorSource {
    *values() {
      yield* values;
    }
  }

  const consumeNext = (iter, next, sum) =>
    next.done ? sum : consumeNext(iter, iter.next(), sum + next.value);

  bench("manual next over object generator", {
    setup: makeObjectGenerator,
    run: (source) => {
      const iter = source.values();
      return consumeNext(iter, iter.next(), 0);
    },
  });

  bench("for...of over object generator", {
    setup: makeObjectGenerator,
    run: (source) => {
      let sum = 0;
      for (const value of source.values()) {
        sum = sum + value;
      }
      return sum;
    },
  });

  bench("yield delegation", {
    setup: makeObjectGenerator,
    run: (source) => {
      let count = 0;
      for (const value of source.values()) {
        count = count + 1;
      }
      return count;
    },
  });

  bench("class generator method", {
    setup: () => new GeneratorSource(),
    run: (source) => {
      let sum = 0;
      for (const value of source.values()) {
        sum = sum + value;
      }
      return sum;
    },
  });
});
