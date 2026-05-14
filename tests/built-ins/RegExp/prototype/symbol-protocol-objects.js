/*---
description: RegExp.prototype Symbol methods accept RegExp protocol objects
features: [RegExp.prototype[Symbol.match], RegExp.prototype[Symbol.matchAll], RegExp.prototype[Symbol.replace], RegExp.prototype[Symbol.search], RegExp.prototype[Symbol.split]]
---*/

const createMatch = (text, index, input) => {
  const match = [text];
  match.index = index;
  match.input = input;
  return match;
};

test("Symbol.match accepts a protocol object with exec", () => {
  const protocol = {
    flags: "",
    lastIndex: 0,
    exec(input) {
      expect(input).toBe("abc");
      return createMatch("b", 1, input);
    },
  };

  expect(RegExp.prototype[Symbol.match].call(protocol, "abc")[0]).toBe("b");
});

test("Symbol.match advances empty protocol matches", () => {
  const protocol = {
    flags: "g",
    lastIndex: 0,
    exec(input) {
      if (this.lastIndex > input.length) {
        return null;
      }
      return createMatch("", this.lastIndex, input);
    },
  };

  expect(RegExp.prototype[Symbol.match].call(protocol, "ab")).toEqual(["", "", ""]);
});

test("Symbol.replace accepts a protocol object with exec", () => {
  const protocol = {
    flags: "",
    lastIndex: 0,
    exec(input) {
      expect(input).toBe("abc");
      return createMatch("b", 1, input);
    },
  };

  expect(RegExp.prototype[Symbol.replace].call(protocol, "abc", "x")).toBe("axc");
});

test("Symbol.replace advances empty protocol matches", () => {
  const protocol = {
    flags: "g",
    lastIndex: 0,
    exec(input) {
      if (this.lastIndex > input.length) {
        return null;
      }
      return createMatch("", this.lastIndex, input);
    },
  };

  expect(RegExp.prototype[Symbol.replace].call(protocol, "ab", "x")).toBe("xaxbx");
});

test("Symbol.replace accepts plain object exec results", () => {
  const protocol = {
    flags: "",
    lastIndex: 0,
    exec(input) {
      return { 0: "b", index: 1, input, length: 1 };
    },
  };

  expect(RegExp.prototype[Symbol.replace].call(protocol, "abc", "x")).toBe("axc");
});

test("Symbol.search accepts a protocol object with exec", () => {
  const protocol = {
    flags: "",
    lastIndex: 0,
    exec(input) {
      expect(input).toBe("abc");
      return createMatch("b", 1, input);
    },
  };

  expect(RegExp.prototype[Symbol.search].call(protocol, "abc")).toBe(1);
});

test("Symbol.search restores protocol object lastIndex", () => {
  const protocol = {
    flags: "",
    lastIndex: 7,
    exec(input) {
      this.lastIndex = 99;
      return createMatch("b", 1, input);
    },
  };

  expect(RegExp.prototype[Symbol.search].call(protocol, "abc")).toBe(1);
  expect(protocol.lastIndex).toBe(7);
});

test("Symbol.split accepts a protocol object with exec", () => {
  let calls = 0;
  const protocol = {
    flags: "",
    lastIndex: 0,
    exec(input) {
      calls++;
      expect(input).toBe("a,b");
      if (calls === 1) {
        return createMatch(",", 1, input);
      }
      return null;
    },
  };

  expect(RegExp.prototype[Symbol.split].call(protocol, "a,b")).toEqual(["a", "b"]);
});

test("Symbol.split sets lastIndex before protocol exec retries", () => {
  const seen = [];
  const protocol = {
    flags: "",
    lastIndex: 0,
    exec(input) {
      seen.push(this.lastIndex);
      if (this.lastIndex === 1) {
        this.lastIndex = 2;
        return createMatch(",", 1, input);
      }
      return null;
    },
  };

  expect(RegExp.prototype[Symbol.split].call(protocol, "a,b")).toEqual(["a", "b"]);
  expect(seen).toEqual([0, 1, 2]);
});

test("Symbol.matchAll accepts a protocol object with exec", () => {
  let calls = 0;
  const protocol = {
    flags: "g",
    lastIndex: 0,
    exec(input) {
      calls++;
      expect(input).toBe("aba");
      if (calls === 1) {
        this.lastIndex = 1;
        return createMatch("a", 0, input);
      }
      if (calls === 2) {
        this.lastIndex = 3;
        return createMatch("a", 2, input);
      }
      return null;
    },
  };

  const matches = [...RegExp.prototype[Symbol.matchAll].call(protocol, "aba")];
  expect(matches[0][0]).toBe("a");
  expect(matches[1].index).toBe(2);
});

test("Symbol.matchAll advances empty protocol matches", () => {
  const protocol = {
    flags: "g",
    lastIndex: 0,
    exec(input) {
      if (this.lastIndex > input.length) {
        return null;
      }
      return createMatch("", this.lastIndex, input);
    },
  };

  const matches = [...RegExp.prototype[Symbol.matchAll].call(protocol, "ab")];
  expect(matches.map((match) => match.index)).toEqual([0, 1, 2]);
});
