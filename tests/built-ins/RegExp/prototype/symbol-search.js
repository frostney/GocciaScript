/*---
description: RegExp.prototype[Symbol.search]
features: [RegExp.prototype[Symbol.search]]
---*/

test("Symbol.search converts input before reading lastIndex", () => {
  const log = [];
  const protocol = {
    get lastIndex() {
      log.push("get-lastIndex");
      return 0;
    },
    set lastIndex(value) {
      log.push("set-lastIndex:" + value);
    },
    exec(input) {
      log.push("exec:" + input);
      return null;
    },
  };
  const input = {
    toString() {
      log.push("string");
      return "abc";
    },
  };

  expect(RegExp.prototype[Symbol.search].call(protocol, input)).toBe(-1);
  expect(log).toEqual(["string", "get-lastIndex", "exec:abc", "get-lastIndex"]);
});

test("Symbol.search returns protocol result index as an observable property", () => {
  const protocol = {
    lastIndex: 0,
    exec(input) {
      return {
        0: "b",
        get index() {
          return "position";
        },
        input,
        length: 1,
      };
    },
  };

  expect(RegExp.prototype[Symbol.search].call(protocol, "abc")).toBe("position");
});

describe.runIf(typeof Goccia !== "undefined")("Symbol.search across a collection", () => {
  // Reuses the memory a wrongly collected object leaves behind.
  const churn = () => Array.from({ length: 2000 }, (_, i) => [i, { i }]);

  test("restores a lastIndex object that only the search still holds", () => {
    const protocol = {
      lastIndex: { previous: ["kept"] },
      exec() {
        Goccia.gc();
        churn();
        return null;
      },
    };
    expect(RegExp.prototype[Symbol.search].call(protocol, "abc")).toBe(-1);
    expect(protocol.lastIndex.previous[0]).toBe("kept");
  });

  test("keeps the match result while the lastIndex read collects", () => {
    let reads = 0;
    const protocol = {
      get lastIndex() {
        reads += 1;
        if (reads === 2) {
          Goccia.gc();
          churn();
        }
        return 0;
      },
      set lastIndex(value) {},
      exec() {
        return { index: [4, 5].length };
      },
    };
    expect(RegExp.prototype[Symbol.search].call(protocol, "abc")).toBe(2);
  });
});
