/*---
description: for...of benchmarks
---*/

import { bench, group } from "goccia:microbench";

group("for...of", () => {
  const smallArray = Array.from({ length: 10 }, (_, i) => i);
  const mediumArray = Array.from({ length: 100 }, (_, i) => i);

  bench("for...of with 10-element array", () => {
    let sum = 0;
    for (const item of smallArray) {
      sum = sum + item;
    }
  });

  bench("for...of with 100-element array", () => {
    let sum = 0;
    for (const item of mediumArray) {
      sum = sum + item;
    }
  });

  bench("for...of with string (10 chars)", () => {
    const result = [];
    for (const ch of "abcdefghij") {
      result.push(ch);
    }
  });

  bench("for...of with Set (10 elements)", ({ *setup() {
    const s = (() => new Set(smallArray))();
    yield () => {
      let sum = 0;
      for (const item of s) {
        sum = sum + item;
      }
    };
  } }).setup);

  bench("for...of with Map entries (10 entries)", ({ *setup() {
    const m = (() => new Map(smallArray.map((v) => [v, v * 2])))();
    yield () => {
      let sum = 0;
      for (const [k, v] of m) {
        sum = sum + k + v;
      }
    };
  } }).setup);

  bench("for...of with destructuring", ({ *setup() {
    const pairs = (() => smallArray.map((v) => [v, v * 10]))();
    yield () => {
      let sum = 0;
      for (const [a, b] of pairs) {
        sum = sum + a + b;
      }
    };
  } }).setup);

  bench("for-await-of with sync array", async () => {
    let sum = 0;
    for await (const item of smallArray) {
      sum = sum + item;
    }
    return sum;
  });
});
