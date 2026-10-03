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

test("Symbol.replace normalizes missing custom result properties to undefined", () => {
  let replacerArgs;
  const protocol = {
    flags: "",
    lastIndex: 0,
    exec() {
      return { 0: "a", length: 2 };
    },
  };

  expect(RegExp.prototype[Symbol.replace].call(
    protocol,
    "a",
    (...args) => {
      replacerArgs = args;
      return "x";
    },
  )).toBe("x");
  expect(replacerArgs).toEqual(["a", undefined, 0, "a"]);
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

test("Symbol.replace calls an exec installed on RegExp.prototype for every global match", () => {
  const originalExec = RegExp.prototype.exec;
  const lastIndexes = [];
  RegExp.prototype.exec = {
    exec(input) {
      lastIndexes.push(this.lastIndex);
      return originalExec.call(this, input);
    },
  }.exec;
  let result;
  try {
    result = "aXbX".replace(/X/g, "-");
  } finally {
    RegExp.prototype.exec = originalExec;
  }
  expect(result).toBe("a-b-");
  expect(lastIndexes).toEqual([0, 2, 4]);
});

test("Symbol.replace reads an exec getter on RegExp.prototype for every exec call", () => {
  const descriptor = Object.getOwnPropertyDescriptor(RegExp.prototype, "exec");
  let reads = 0;
  Object.defineProperty(RegExp.prototype, "exec", {
    configurable: true,
    get() {
      reads++;
      return descriptor.value;
    },
  });
  let result;
  try {
    result = "aaa".replace(/a/g, "b");
  } finally {
    Object.defineProperty(RegExp.prototype, "exec", descriptor);
  }
  expect(result).toBe("bbb");
  expect(reads).toBe(4);
});

test("Symbol.replace calls an own exec of a global regex for every match", () => {
  const regex = /a/g;
  let calls = 0;
  regex.exec = (input) => {
    calls++;
    return RegExp.prototype.exec.call(regex, input);
  };
  expect("aa".replace(regex, "b")).toBe("bb");
  expect(calls).toBe(3);
});

test("Symbol.replace calls the replacer after all matches, with lastIndex already 0", () => {
  const regex = /a(\d)?/g;
  regex.lastIndex = 5;
  const calls = [];
  const result = "a1-a-a2".replace(regex, (match, digit, offset, input) => {
    calls.push([match, digit, offset, input, regex.lastIndex]);
    return "<" + match + ">";
  });
  expect(result).toBe("<a1>-<a>-<a2>");
  expect(calls).toEqual([
    ["a1", "1", 0, "a1-a-a2", 0],
    ["a", undefined, 3, "a1-a-a2", 0],
    ["a2", "2", 5, "a1-a-a2", 0],
  ]);
  expect(regex.lastIndex).toBe(0);
});

test("Symbol.replace expands templates for every global match", () => {
  expect("a1b2".replace(/[a-z](\d)/g, "[$&|$1|$`|$']")).toBe(
    "[a1|1||b2][b2|2|a1|]");
  expect("abc".replace(/b/g, "$$")).toBe("a$c");
  expect("aaa".replace(/a/g, "")).toBe("");
});

test("Symbol.replace advances empty matches by the unicode property", () => {
  const regex = /(?:)/g;
  Object.defineProperty(regex, "unicode", { value: true });
  expect("\u{1F600}".replace(regex, "-")).toBe("-\u{1F600}-");
  expect("\u{1F600}".replace(/(?:)/g, "-")).toBe("-\ud83d-\ude00-");
});

test("Symbol.replace and Symbol.match call exec from a prototype between the regex and RegExp.prototype", () => {
  const calls = [];
  const makeRegex = () => {
    const regex = /a/g;
    Object.setPrototypeOf(regex, Object.create(RegExp.prototype, {
      exec: {
        value(input) {
          calls.push(this.lastIndex);
          return RegExp.prototype.exec.call(this, input);
        },
      },
    }));
    return regex;
  };
  expect("aXa".replace(makeRegex(), "b")).toBe("bXb");
  expect(calls).toEqual([0, 1, 3]);
  calls.length = 0;
  expect("aXa".match(makeRegex())).toEqual(["a", "a"]);
  expect(calls).toEqual([0, 1, 3]);
});
