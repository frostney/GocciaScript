/*---
description: RegExp.prototype[Symbol.split]
features: [RegExp.prototype[Symbol.split]]
---*/

test("Symbol.split coerces limit with ToUint32", () => {
  expect(/,/[Symbol.split]("a,b,c", NaN)).toEqual([]);
  expect(/,/[Symbol.split]("a,b,c", Infinity)).toEqual([]);
  expect(/,/[Symbol.split]("a,b,c", -Infinity)).toEqual([]);
  expect(/,/[Symbol.split]("a,b,c", 4294967297)).toEqual(["a"]);
  expect(/,/[Symbol.split]("a,b,c", -4294967295)).toEqual(["a"]);
});

test("Symbol.split converts input before species, flags, and limit", () => {
  const log = [];
  class Source extends RegExp {
    static get [Symbol.species]() {
      log.push("species");
      return class Splitter extends RegExp {
        constructor(rx, flags) {
          log.push("construct:" + flags);
          super(rx, flags);
        }
      };
    }
  }
  const regex = new Source(",");
  const input = {
    toString() {
      log.push("string");
      return "a,b";
    },
  };
  const limit = {
    valueOf() {
      log.push("limit");
      return 2;
    },
  };

  expect(regex[Symbol.split](input, limit)).toEqual(["a", "b"]);
  expect(log).toEqual(["string", "species", "construct:y", "limit"]);
});

test("Symbol.split treats undefined limit like an omitted limit", () => {
  expect(/,/[Symbol.split]("a,b,c", undefined)).toEqual(["a", "b", "c"]);
});

test("Symbol.split constructs sticky splitter through Symbol.species", () => {
  class Splitter extends RegExp {
    constructor(rx, flags) {
      super("x", flags);
    }
  }

  class Source extends RegExp {
    static get [Symbol.species]() {
      return Splitter;
    }
  }

  expect(new Source(",")[Symbol.split]("a,bxc")).toEqual(["a,b", "c"]);
});

test("Symbol.split rejects non-constructor Symbol.species", () => {
  class Source extends RegExp {
    static get [Symbol.species]() {
      return {};
    }
  }

  expect(() => new Source(",")[Symbol.split]("a,b")).toThrow(TypeError);
});

test("Symbol.split handles empty input matches", () => {
  expect(/(?:)/[Symbol.split]("")).toEqual([]);
  expect(/.?/[Symbol.split]("")).toEqual([]);
});

test("Symbol.split advances zero-width unicode matches by code point", () => {
  const input = "\uD83D\uDC38\uD83D\uDC39X\uD83D\uDC3A";

  expect(/\uD83D|X|/u[Symbol.split](input)).toEqual([
    "\uD83D\uDC38",
    "\uD83D\uDC39",
    "\uD83D\uDC3A",
  ]);
  expect(/\uDC38|X|/u[Symbol.split](input)).toEqual([
    "\uD83D\uDC38",
    "\uD83D\uDC39",
    "\uD83D\uDC3A",
  ]);
  expect(/\uD83D\uDC38|X|/u[Symbol.split](input)).toEqual([
    "",
    "\uD83D\uDC39",
    "\uD83D\uDC3A",
  ]);
});

test("Symbol.split calls an exec installed on RegExp.prototype at every position", () => {
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
    result = "a,b".split(/,/);
  } finally {
    RegExp.prototype.exec = originalExec;
  }
  expect(result).toEqual(["a", "b"]);
  expect(lastIndexes).toEqual([0, 1, 2]);
});

test("Symbol.split includes captures and stops at the limit", () => {
  expect("a1b2c".split(/(\d)/)).toEqual(["a", "1", "b", "2", "c"]);
  expect("a1b2c".split(/(\d)|x/)).toEqual(["a", "1", "b", "2", "c"]);
  expect("axb".split(/(\d)|x/)).toEqual(["a", undefined, "b"]);
  expect("a1b2c".split(/(\d)/, 2)).toEqual(["a", "1"]);
  expect("a1b2c".split(/(\d)/, 3)).toEqual(["a", "1", "b"]);
  expect("abc".split(/(?:)/)).toEqual(["a", "b", "c"]);
  expect(",a,".split(/,/)).toEqual(["", "a", ""]);
});

test("Symbol.split sets lastIndex on a splitter that a species constructor kept", () => {
  let splitter;
  class Species {
    constructor(source, flags) {
      splitter = new RegExp(source, flags);
      return splitter;
    }
  }
  const regex = /,/;
  regex.constructor = { [Symbol.species]: Species };
  expect("a,b,".split(regex)).toEqual(["a", "b", ""]);
  expect(splitter.lastIndex).toBe(4);
  expect("a,b,c".split(regex, 1)).toEqual(["a"]);
  expect(splitter.lastIndex).toBe(2);
});

test("Symbol.split throws when the splitter's lastIndex is not writable", () => {
  class Species {
    constructor(source, flags) {
      const splitter = new RegExp(source, flags);
      Object.defineProperty(splitter, "lastIndex", { writable: false, value: 0 });
      return splitter;
    }
  }
  const regex = /,/;
  regex.constructor = { [Symbol.species]: Species };
  expect(() => "a,b".split(regex)).toThrow(TypeError);
});
