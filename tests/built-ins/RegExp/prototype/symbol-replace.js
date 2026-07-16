/*---
description: RegExp.prototype[Symbol.replace]
features: [RegExp.prototype[Symbol.replace]]
---*/

test("Symbol.replace converts input and replacement before reading flags", () => {
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
  const replacement = {
    toString() {
      log.push("replaceValue");
      return "x";
    },
  };

  expect(RegExp.prototype[Symbol.replace].call(protocol, input, replacement)).toBe("abc");
  expect(log).toEqual(["string", "replaceValue", "flags", "exec:abc"]);
});

test("Symbol.replace uses ToLength when advancing lastIndex after empty global matches", () => {
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

  expect(RegExp.prototype[Symbol.replace].call(protocol, "a", "x")).toBe("xa");
  expect(log).toEqual([
    "set-lastIndex:0",
    "get-lastIndex",
    "valueOf-lastIndex",
    "set-lastIndex:1",
  ]);
});

test("Symbol.replace retains custom exec results until replacement processing", () => {
  const log = [];
  let calls = 0;
  const match = {
    get length() {
      log.push("length");
      return 2;
    },
    get 0() {
      log.push("match");
      return "b";
    },
    get 1() {
      log.push("capture");
      return "b";
    },
    get index() {
      log.push("index");
      return 1;
    },
    get groups() {
      log.push("groups");
      return undefined;
    },
  };
  const protocol = {
    flags: "g",
    lastIndex: 0,
    exec() {
      calls++;
      log.push("exec:" + calls);
      if (calls === 1) {
        this.lastIndex = 2;
        return match;
      }
      return null;
    },
  };

  expect(RegExp.prototype[Symbol.replace].call(
    protocol,
    "abc",
    (matched, capture) => matched + capture,
  )).toBe("abbc");
  expect(log).toEqual([
    "exec:1",
    "match",
    "exec:2",
    "length",
    "match",
    "index",
    "capture",
    "groups",
  ]);
});

test("Symbol.replace preserves retained results when groups aliases a later match", () => {
  let calls = 0;
  const protocol = {
    flags: "g",
    lastIndex: 0,
    pending: null,
    exec(input) {
      calls++;
      if (calls === 1) {
        this.pending = {
          get length() {
            return {
              valueOf() {
                Goccia.gc();
                return 1;
              },
            };
          },
          0: "b",
          index: 1,
          groups: undefined,
        };
        this.lastIndex = 1;
        return {
          0: "a",
          index: 0,
          length: 1,
          get groups() {
            const result = protocol.pending;
            protocol.pending = null;
            return result;
          },
        };
      }
      if (calls === 2) {
        this.lastIndex = 2;
        return this.pending;
      }
      return null;
    },
  };

  expect(RegExp.prototype[Symbol.replace].call(
    protocol,
    "ab",
    "x",
  )).toBe("xx");
});
