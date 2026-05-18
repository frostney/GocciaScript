/*---
description: RegExp.prototype[Symbol.match]
features: [RegExp.prototype[Symbol.match]]
---*/

test("Symbol.match converts input before reading flags", () => {
  const log = [];
  const protocol = {
    get flags() {
      log.push("flags");
      return "g";
    },
    lastIndex: 0,
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

  expect(RegExp.prototype[Symbol.match].call(protocol, input)).toBe(null);
  expect(log).toEqual(["string", "flags", "exec:abc"]);
});

test("Symbol.match uses ToLength when advancing lastIndex after empty global matches", () => {
  const log = [];
  let execCalls = 0;
  let storedLastIndex = 0;
  const protocol = {
    flags: "g",
    get lastIndex() {
      log.push("get-lastIndex");
      return {
        valueOf() {
          log.push("valueOf-lastIndex");
          return storedLastIndex;
        },
      };
    },
    set lastIndex(value) {
      storedLastIndex = value;
      log.push("set-lastIndex:" + value);
    },
    exec(input) {
      if (execCalls++ > 0) {
        return null;
      }
      return { 0: "", index: 0, input, length: 1 };
    },
  };

  expect(RegExp.prototype[Symbol.match].call(protocol, "a")).toEqual([""]);
  expect(log).toEqual([
    "set-lastIndex:0",
    "get-lastIndex",
    "valueOf-lastIndex",
    "set-lastIndex:1",
  ]);
});
