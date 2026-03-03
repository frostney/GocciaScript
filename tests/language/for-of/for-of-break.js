/*---
description: for...of with break statement
features: [for-of]
---*/

describe("for...of break", () => {
  test("break exits the loop early", () => {
    const result = [];
    for (const item of [1, 2, 3, 4, 5]) {
      if (item === 3) {
        break;
      }
      result.push(item);
    }
    expect(result).toEqual([1, 2]);
  });

  test("break in nested for...of", () => {
    const result = [];
    for (const outer of ["a", "b"]) {
      for (const inner of [1, 2, 3]) {
        if (inner === 2) {
          break;
        }
        result.push(outer + inner);
      }
    }
    expect(result).toEqual(["a1", "b1"]);
  });

  test("break inside try-finally executes finally block", () => {
    const log = [];
    for (const x of [1, 2, 3]) {
      try {
        if (x === 2) {
          break;
        }
        log.push("body:" + x);
      } finally {
        log.push("finally:" + x);
      }
    }
    expect(log).toEqual(["body:1", "finally:1", "finally:2"]);
  });

  test("break inside nested try-finally executes all finally blocks", () => {
    const log = [];
    for (const x of [1, 2]) {
      try {
        try {
          if (x === 1) {
            break;
          }
        } finally {
          log.push("inner:" + x);
        }
      } finally {
        log.push("outer:" + x);
      }
    }
    expect(log).toEqual(["inner:1", "outer:1"]);
  });

  test("break inside try-finally inside outer try-finally", () => {
    const log = [];
    try {
      for (const x of [1, 2, 3]) {
        try {
          if (x === 2) {
            break;
          }
          log.push("body:" + x);
        } finally {
          log.push("loop-finally:" + x);
        }
      }
    } finally {
      log.push("outer-finally");
    }
    expect(log).toEqual(["body:1", "loop-finally:1", "loop-finally:2", "outer-finally"]);
  });
});
