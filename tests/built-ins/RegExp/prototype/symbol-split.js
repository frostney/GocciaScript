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
