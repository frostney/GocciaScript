/*---
description: Module import benchmarks
---*/

import { bench, group } from "goccia:microbench";

import { add, multiply, PI, greeting } from "./helpers/bench-module.js";
import { name, version, debug, maxRetries, tags } from "./helpers/bench-config.json";

group("script module import", () => {
  bench("call imported function", () => {
    const r = add(2, 3);
  });

  bench("call two imported functions", () => {
    const r1 = add(2, 3);
    const r2 = multiply(4, 5);
  });

  bench("read imported constant", () => {
    const r = PI;
  });

  bench("read imported string", () => {
    const r = greeting;
  });
});

group("JSON module import", () => {
  bench("read JSON string property", () => {
    const r = name;
  });

  bench("read JSON number property", () => {
    const r = maxRetries;
  });

  bench("read JSON boolean property", () => {
    const r = debug;
  });

  bench("read JSON array property", () => {
    const r = tags;
  });

  bench("read multiple JSON properties", () => {
    const r1 = name;
    const r2 = version;
    const r3 = maxRetries;
  });
});
