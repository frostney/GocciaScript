/*---
description: RegExp.prototype[Symbol.matchAll]
features: [RegExp.prototype[Symbol.matchAll]]
---*/

test("Symbol.matchAll returns a single match for non-global regexes", () => {
  const regex = /a/;
  const matches = [];

  for (const match of regex[Symbol.matchAll]("ba")) {
    matches.push([match[0], match.index]);
  }

  expect(matches).toEqual([["a", 1]]);
});

test("Symbol.matchAll converts input before species and flags", () => {
  const log = [];
  class Source extends RegExp {
    static get [Symbol.species]() {
      log.push("species");
      return class Matcher extends RegExp {
        constructor(rx, flags) {
          log.push("construct:" + flags);
          super(rx, flags);
        }
      };
    }
  }
  const regex = new Source("a", "g");
  const input = {
    toString() {
      log.push("string");
      return "a";
    },
  };

  regex[Symbol.matchAll](input).next();
  expect(log).toEqual(["string", "species", "construct:g"]);
});

test("Symbol.matchAll sets matcher lastIndex from ToLength of the original", () => {
  const log = [];
  const matcher = {
    set lastIndex(value) {
      log.push("matcher-lastIndex:" + value);
    },
  };
  class Source extends RegExp {
    static get [Symbol.species]() {
      return class Matcher {
        constructor() {
          return matcher;
        }
      };
    }
  }
  const regex = new Source("a", "g");
  regex.lastIndex = {
    valueOf() {
      log.push("valueOf-lastIndex");
      return 1;
    },
  };

  regex[Symbol.matchAll]("aaa");
  expect(log).toEqual(["valueOf-lastIndex", "matcher-lastIndex:1"]);
});

test("Symbol.matchAll does not mutate the original regex lastIndex", () => {
  const regex = /a/g;

  regex.lastIndex = 1;
  for (const _match of regex[Symbol.matchAll]("aba")) {
  }

  expect(regex.lastIndex).toBe(1);
});

test("Symbol.match resets lastIndex on global regexes", () => {
  const regex = /a/g;

  regex.lastIndex = 1;
  expect(regex[Symbol.match]("aba")).toEqual(["a", "a"]);
  expect(regex.lastIndex).toBe(0);
});

test("Symbol.replace resets lastIndex on global regexes", () => {
  const regex = /a/g;

  regex.lastIndex = 1;
  expect("aba".replace(regex, "x")).toBe("xbx");
  expect(regex.lastIndex).toBe(0);
});

test("Symbol.match respects sticky lastIndex and updates it", () => {
  const regex = /a/y;

  regex.lastIndex = 1;
  expect(regex[Symbol.match]("ba")[0]).toBe("a");
  expect(regex.lastIndex).toBe(2);
});

test("Symbol.replace respects sticky lastIndex and updates it", () => {
  const regex = /a/y;

  regex.lastIndex = 1;
  expect("ba".replace(regex, "x")).toBe("bx");
  expect(regex.lastIndex).toBe(2);
});

test("Symbol.matchAll returns a lazy iterator", () => {
  const regex = /a/g;
  const iter = regex[Symbol.matchAll]("aaa");

  const first = iter.next();
  expect(first.done).toBe(false);
  expect(first.value[0]).toBe("a");
  expect(first.value.index).toBe(0);

  const second = iter.next();
  expect(second.done).toBe(false);
  expect(second.value.index).toBe(1);

  const third = iter.next();
  expect(third.done).toBe(false);
  expect(third.value.index).toBe(2);

  const fourth = iter.next();
  expect(fourth.done).toBe(true);
});

test("Symbol.matchAll with global and sticky flags iterates all matches", () => {
  const regex = /a/gy;
  const matches = [...regex[Symbol.matchAll]("aab")];

  expect(matches.length).toBe(2);
  expect(matches[0][0]).toBe("a");
  expect(matches[1][0]).toBe("a");
});

test("Symbol.matchAll with sticky-only yields a single match", () => {
  const regex = /a/y;
  const matches = [...regex[Symbol.matchAll]("aaa")];

  // Sticky without global: iterator yields one match then stops
  expect(matches.length).toBe(1);
  expect(matches[0][0]).toBe("a");
  expect(matches[0].index).toBe(0);
});

test("Symbol.matchAll preserves lastIndex from cloned regex", () => {
  const regex = /a/g;
  regex.lastIndex = 1;

  const matches = [...regex[Symbol.matchAll]("aaa")];

  // Iterator starts from cloned lastIndex (1), so first match is at index 1
  expect(matches.length).toBe(2);
  expect(matches[0].index).toBe(1);
  expect(matches[1].index).toBe(2);

  // Original regex lastIndex is not mutated
  expect(regex.lastIndex).toBe(1);
});

test("Symbol.matchAll gives every match the subject as input", () => {
  const input = "a1-b2";
  const matches = [...input.matchAll(/([a-z])(\d)/g)];
  expect(matches.map((match) => match.index)).toEqual([0, 3]);
  expect(matches.map((match) => match.input)).toEqual([input, input]);
  expect(matches.map((match) => match[2])).toEqual(["1", "2"]);
  expect(matches[0].groups).toBeUndefined();
});

test("Symbol.matchAll advances past empty matches", () => {
  expect([..."ab".matchAll(/(?:)/g)].map((match) => match.index)).toEqual([0, 1, 2]);
});

test("Symbol.matchAll calls an exec installed during iteration with the matcher's lastIndex", () => {
  const iterator = "a1a2a3".matchAll(/a\d/g);
  const first = iterator.next().value[0];
  const originalExec = RegExp.prototype.exec;
  const lastIndexes = [];
  RegExp.prototype.exec = {
    exec(input) {
      lastIndexes.push(this.lastIndex);
      return originalExec.call(this, input);
    },
  }.exec;
  let rest;
  try {
    rest = [...iterator].map((match) => match[0]);
  } finally {
    RegExp.prototype.exec = originalExec;
  }
  expect(first).toBe("a1");
  expect(rest).toEqual(["a2", "a3"]);
  expect(lastIndexes).toEqual([2, 4, 6]);
});

test("Symbol.matchAll reads the lastIndex of a matcher from a custom species", () => {
  let matcher;
  class Species {
    constructor(source, flags) {
      matcher = new RegExp(source, flags);
      return matcher;
    }
  }
  const regex = /a/g;
  regex.constructor = { [Symbol.species]: Species };
  const iterator = regex[Symbol.matchAll]("aaaa");
  expect(iterator.next().value.index).toBe(0);
  matcher.lastIndex = 3;
  expect(iterator.next().value.index).toBe(3);
  expect(iterator.next().done).toBe(true);
});

test("Symbol.matchAll keeps lastIndex of a matcher that an earlier exec call exposed", () => {
  const originalExec = RegExp.prototype.exec;
  let matcher;
  RegExp.prototype.exec = {
    exec(input) {
      matcher = this;
      return originalExec.call(this, input);
    },
  }.exec;
  const iterator = "aaaa".matchAll(/a/g);
  let first;
  try {
    first = iterator.next().value.index;
  } finally {
    RegExp.prototype.exec = originalExec;
  }
  expect(first).toBe(0);
  expect(matcher.lastIndex).toBe(1);
  expect(iterator.next().value.index).toBe(1);
  expect(matcher.lastIndex).toBe(2);
  matcher.lastIndex = 0;
  expect(iterator.next().value.index).toBe(0);
  expect(matcher.lastIndex).toBe(1);
});
