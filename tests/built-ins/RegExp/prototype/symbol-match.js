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

test("Symbol.match does not coerce lastIndex after non-empty custom exec matches", () => {
  const regex = /./g;
  let nextMatch = "a non-empty string";

  regex.exec = () => {
    const thisMatch = nextMatch;
    if (thisMatch === null) {
      return null;
    }
    nextMatch = null;
    return {
      get 0() {
        regex.lastIndex = {
          valueOf() {
            throw new Error("lastIndex should not be coerced");
          },
        };
        return thisMatch;
      },
    };
  };

  expect(regex[Symbol.match]("")).toEqual(["a non-empty string"]);
});

test("Symbol.match stringifies a missing custom result match as undefined", () => {
  let calls = 0;
  const protocol = {
    flags: "g",
    lastIndex: 0,
    exec() {
      if (calls++ === 0) {
        return {};
      }
      return null;
    },
  };

  expect(RegExp.prototype[Symbol.match].call(protocol, "")).toEqual(["undefined"]);
});

test("Symbol.match retains custom results while reading matched text", () => {
  let calls = 0;
  const protocol = {
    flags: "g",
    lastIndex: 0,
    exec() {
      if (calls++ > 0) {
        return null;
      }
      return {
        get 0() {
          Goccia.gc();
          return "a";
        },
      };
    },
  };

  expect(RegExp.prototype[Symbol.match].call(protocol, "")).toEqual(["a"]);
});

test("Symbol.match returns only matched strings for native global results", () => {
  expect(/a(b)/dg[Symbol.match]("abxab")).toEqual(["ab", "ab"]);
});

test("Symbol.match calls an exec installed on RegExp.prototype for every global match", () => {
  const originalExec = RegExp.prototype.exec;
  let calls = 0;
  RegExp.prototype.exec = {
    exec(input) {
      calls++;
      return originalExec.call(this, input);
    },
  }.exec;
  let result;
  try {
    result = "a1b22c".match(/\d+/g);
  } finally {
    RegExp.prototype.exec = originalExec;
  }
  expect(result).toEqual(["1", "22"]);
  expect(calls).toBe(3);
});

test("Symbol.match collects empty global matches and resets lastIndex", () => {
  const regex = /x*/g;
  regex.lastIndex = 3;
  expect("axxb".match(regex)).toEqual(["", "xx", "", ""]);
  expect(regex.lastIndex).toBe(0);
});

test("Symbol.match advances empty matches by the unicode property", () => {
  const regex = /(?:)/g;
  Object.defineProperty(regex, "unicode", { value: true });
  expect("\u{1F600}".match(regex).length).toBe(2);
  expect("\u{1F600}".match(/(?:)/g).length).toBe(3);
});
