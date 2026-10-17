/*---
description: RegExp.prototype.test
features: [RegExp.prototype.test]
---*/

test("test returns true when a match exists", () => {
  expect(/abc/.test("zabcz")).toBe(true);
});

test("test returns false when a match does not exist", () => {
  expect(/abc/.test("zzz")).toBe(false);
});

test("test respects global lastIndex", () => {
  const regex = /a/g;

  expect(regex.test("aba")).toBe(true);
  expect(regex.lastIndex).toBe(1);
  expect(regex.test("aba")).toBe(true);
  expect(regex.lastIndex).toBe(3);
  expect(regex.test("aba")).toBe(false);
  expect(regex.lastIndex).toBe(0);
});

test("test uses generic exec protocol for object receivers", () => {
  const receiver = {
    exec(value) {
      expect(value).toBe("input");
      return {};
    },
  };

  expect(RegExp.prototype.test.call(receiver, "input")).toBe(true);
});

test("test throws when generic exec returns a primitive", () => {
  const receiver = {
    exec() {
      return 1;
    },
  };

  expect(() => {
    RegExp.prototype.test.call(receiver, "input");
  }).toThrow(TypeError);
});

test("test on the built-in exec honours sticky lastIndex", () => {
  const regex = /b/y;

  regex.lastIndex = 1;
  expect(regex.test("ab")).toBe(true);
  expect(regex.lastIndex).toBe(2);
  regex.lastIndex = 0;
  expect(regex.test("ab")).toBe(false);
  expect(regex.lastIndex).toBe(0);
});

test("test fails and resets lastIndex when it is past the end", () => {
  const regex = /a/g;
  regex.lastIndex = 10;

  expect(regex.test("aba")).toBe(false);
  expect(regex.lastIndex).toBe(0);
});

test("test reads lastIndex once even without the global flag", () => {
  const regex = /a/;
  let reads = 0;
  regex.lastIndex = {
    valueOf() {
      reads = reads + 1;
      return 2;
    },
  };

  expect(regex.test("aba")).toBe(true);
  expect(reads).toBe(1);
});

test("test calls an own exec on a RegExp instance", () => {
  const regex = /a/;
  const seen = [];
  regex.exec = (value) => {
    seen.push(value);
    return null;
  };

  expect(regex.test("aaa")).toBe(false);
  expect(seen).toEqual(["aaa"]);
});

test("test calls a replaced RegExp.prototype.exec", () => {
  const original = RegExp.prototype.exec;
  const seen = [];
  RegExp.prototype.exec = {
    exec(value) {
      seen.push(value);
      return original.call(this, value);
    },
  }.exec;
  try {
    expect(/b/.test("abc")).toBe(true);
    expect(/z/.test("abc")).toBe(false);
  } finally {
    RegExp.prototype.exec = original;
  }
  expect(seen).toEqual(["abc", "abc"]);
});

test("test calls exec defined on a RegExp subclass", () => {
  const seen = [];
  class Logged extends RegExp {
    exec(value) {
      seen.push(value);
      return super.exec(value);
    }
  }

  expect(new Logged("b").test("abc")).toBe(true);
  expect(seen).toEqual(["abc"]);
});

test("test runs the exec it read before lastIndex's valueOf replaced it", () => {
  const regex = /b/;
  let replaced = 0;
  regex.lastIndex = {
    valueOf() {
      regex.exec = () => {
        replaced = replaced + 1;
        return null;
      };
      return 0;
    },
  };

  expect(regex.test("abc")).toBe(true);
  expect(replaced).toBe(0);
});

test("test reads the flags after lastIndex", () => {
  const regex = /b/y;
  regex.lastIndex = {
    valueOf() {
      regex.compile("b");
      return 0;
    },
  };

  // compile drops the sticky flag before RegExpBuiltinExec reads the flags.
  expect(regex.test("abc")).toBe(true);
});

describe.runIf(typeof Goccia !== "undefined")("test memory charge", () => {
  test("a match does not charge another copy of the subject", () => {
    const subject = "hello world 12345 foo bar ".repeat(640);
    const regex = /o(r)/;
    Goccia.gc();
    const before = Goccia.gc.bytesAllocated;
    for (const i of [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) {
      expect(regex.test(subject)).toBe(true);
    }
    const perCall = (Goccia.gc.bytesAllocated - before) / 10;

    // The subject alone would charge subject.length * 2 bytes.
    expect(perCall).toBeLessThan(subject.length);
  });
});
