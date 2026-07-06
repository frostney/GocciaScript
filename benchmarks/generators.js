/*---
description: Generator benchmarks
---*/

import { bench, group } from "goccia:microbench";

group("generators", () => {
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

  bench("manual next over object generator", ({ *setup() {
    const source = makeObjectGenerator();
    yield () => {
      const iter = source.values();
      return consumeNext(iter, iter.next(), 0);
    };
  } }).setup);

  bench("for...of over object generator", ({ *setup() {
    const source = makeObjectGenerator();
    yield () => {
      let sum = 0;
      for (const value of source.values()) {
        sum = sum + value;
      }
      return sum;
    };
  } }).setup);

  bench("yield delegation", ({ *setup() {
    const source = makeObjectGenerator();
    yield () => {
      let count = 0;
      for (const value of source.values()) {
        count = count + 1;
      }
      return count;
    };
  } }).setup);

  bench("class generator method", ({ *setup() {
    const source = (() => new GeneratorSource())();
    yield () => {
      let sum = 0;
      for (const value of source.values()) {
        sum = sum + value;
      }
      return sum;
    };
  } }).setup);
});
