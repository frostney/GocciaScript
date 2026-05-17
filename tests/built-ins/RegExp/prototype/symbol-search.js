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
