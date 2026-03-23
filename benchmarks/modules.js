/*---
description: Module import benchmarks
---*/

import { add, multiply, PI, greeting } from "./helpers/bench-module.js";
import { name, version, debug, maxRetries, tags } from "./helpers/bench-config.json";

suite("script module import", () => {
  bench("call imported function", {
    run: () => {
      const r = add(2, 3);
    },
  });

  bench("call two imported functions", {
    run: () => {
      const r1 = add(2, 3);
      const r2 = multiply(4, 5);
    },
  });

  bench("read imported constant", {
    run: () => {
      const r = PI;
    },
  });

  bench("read imported string", {
    run: () => {
      const r = greeting;
    },
  });
});

suite("JSON module import", () => {
  bench("read JSON string property", {
    run: () => {
      const r = name;
    },
  });

  bench("read JSON number property", {
    run: () => {
      const r = maxRetries;
    },
  });

  bench("read JSON boolean property", {
    run: () => {
      const r = debug;
    },
  });

  bench("read JSON array property", {
    run: () => {
      const r = tags;
    },
  });

  bench("read multiple JSON properties", {
    run: () => {
      const r1 = name;
      const r2 = version;
      const r3 = maxRetries;
    },
  });
});
