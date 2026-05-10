/*---
description: RegExp constructor
features: [RegExp]
---*/

test("RegExp constructor creates regex objects with flags and properties", () => {
  const regex = new RegExp("ab", "gi");

  expect(regex.source).toBe("ab");
  expect(regex.flags).toBe("gi");
  expect(regex.global).toBe(true);
  expect(regex.ignoreCase).toBe(true);
  expect(regex.multiline).toBe(false);
  expect(regex.dotAll).toBe(false);
  expect(regex.sticky).toBe(false);
  expect(regex.lastIndex).toBe(0);
});

test("RegExp canonicalizes flags and exposes unicode", () => {
  const regex = new RegExp("ab", "yugim");

  expect(regex.flags).toBe("gimuy");
  expect(regex.global).toBe(true);
  expect(regex.ignoreCase).toBe(true);
  expect(regex.multiline).toBe(true);
  expect(regex.unicode).toBe(true);
  expect(regex.sticky).toBe(true);
  expect(regex.toString()).toBe("/ab/gimuy");
});

test("RegExp can be called without new", () => {
  const regex = RegExp("ab", "g");

  expect(regex.test("zabz")).toBe(true);
  expect(regex.flags).toBe("g");
});

test("RegExp clones another regex when passed a regex argument", () => {
  const original = /test/gi;
  const cloned = new RegExp(original);

  expect(cloned.source).toBe("test");
  expect(cloned.flags).toBe("gi");
  expect(cloned.toString()).toBe("/test/gi");
});

test("RegExp called without new returns the same regex when flags are omitted", () => {
  const original = /test/gi;
  const result = RegExp(original);

  expect(result).toBe(original);
});

test("new RegExp clones regex arguments even when flags are omitted", () => {
  const original = /test/gi;
  const cloned = new RegExp(original);

  expect(cloned).not.toBe(original);
  expect(cloned.source).toBe("test");
  expect(cloned.flags).toBe("gi");
});

test("RegExp throws SyntaxError for invalid flags", () => {
  expect(() => {
    new RegExp("a", "gg");
  }).toThrow(SyntaxError);

  expect(() => {
    new RegExp("a", "z");
  }).toThrow(SyntaxError);
});

test("RegExp throws SyntaxError for invalid patterns at construction time", () => {
  expect(() => {
    new RegExp("[");
  }).toThrow(SyntaxError);
});

test("RegExp accepts d flag and exposes hasIndices", () => {
  const regex = new RegExp("a", "dg");
  expect(regex.flags).toBe("dg");
  expect(regex.hasIndices).toBe(true);
});

test("RegExp accepts v flag and exposes unicodeSets", () => {
  const regex = new RegExp("a", "gv");
  expect(regex.flags).toBe("gv");
  expect(regex.unicodeSets).toBe(true);
});

test("RegExp throws for u and v flags together", () => {
  expect(() => {
    new RegExp("a", "uv");
  }).toThrow(SyntaxError);
});

test("RegExp canonicalizes new flags in correct order", () => {
  const regex = new RegExp("a", "yvgdims");
  expect(regex.flags).toBe("dgimsvy");
});

// --- Syntax validation ---

test("dangling quantifier throws SyntaxError", () => {
  expect(() => { new RegExp("a**"); }).toThrow(SyntaxError);
  expect(() => { new RegExp("??"); }).toThrow(SyntaxError);
  expect(() => { new RegExp("+"); }).toThrow(SyntaxError);
  expect(() => { new RegExp("*"); }).toThrow(SyntaxError);
});

test("invalid character class range throws SyntaxError", () => {
  expect(() => { new RegExp("[z-a]"); }).toThrow(SyntaxError);
  expect(() => { new RegExp("[b-ac-e]"); }).toThrow(SyntaxError);
});

test("quantifier min > max throws SyntaxError", () => {
  expect(() => { new RegExp("0{2,1}"); }).toThrow(SyntaxError);
});

test("trailing backslash throws SyntaxError", () => {
  expect(() => { new RegExp("\\"); }).toThrow(SyntaxError);
});

test("huge quantifier does not crash", () => {
  expect(/x{2147483648}x/.test("1")).toBe(false);
});

test("exec on Object.create(RegExp.prototype) throws TypeError", () => {
  const obj = Object.create(RegExp.prototype);
  expect(() => { RegExp.prototype.exec.call(obj, "test"); }).toThrow(TypeError);
});

test("test on Object.create(RegExp.prototype) throws TypeError", () => {
  const obj = Object.create(RegExp.prototype);
  expect(() => { RegExp.prototype.test.call(obj, "test"); }).toThrow(TypeError);
});

// --- IsRegExp via Symbol.match ---

test("new RegExp reads source/flags from object with Symbol.match truthy", () => {
  const obj = { source: "abc", flags: "g", [Symbol.match]: true };
  const r = new RegExp(obj);
  expect(r.source).toBe("abc");
  expect(r.flags).toBe("g");
  expect(r.toString()).toBe("/abc/g");
});

test("new RegExp uses empty strings when source/flags are absent on Symbol.match object", () => {
  const obj = { [Symbol.match]: true };
  const r = new RegExp(obj);
  expect(r.source).toBe("(?:)");
  expect(r.flags).toBe("");
});

test("new RegExp stringifies object when Symbol.match is false", () => {
  const obj = { source: "abc", flags: "", [Symbol.match]: false };
  const r = new RegExp(obj);
  expect(r.source).toBe("[object Object]");
});

test("RegExp() without new returns actual RegExp as-is when flags omitted", () => {
  const r = /abc/g;
  const result = RegExp(r);
  expect(result).toBe(r);
});

test("RegExp() without new wraps Symbol.match object whose constructor differs", () => {
  const obj = { source: "abc", flags: "", [Symbol.match]: true };
  const result = RegExp(obj);
  expect(result).not.toBe(obj);
  expect(result.source).toBe("abc");
});

test("new RegExp overrides flags from Symbol.match object when second arg provided", () => {
  const obj = { source: "abc", flags: "g", [Symbol.match]: true };
  const r = new RegExp(obj, "i");
  expect(r.source).toBe("abc");
  expect(r.flags).toBe("i");
});

test("new RegExp treats Symbol.match with truthy non-boolean as regexp-like", () => {
  const obj = { source: "x", flags: "", [Symbol.match]: 1 };
  const r = new RegExp(obj);
  expect(r.source).toBe("x");
});

test("new RegExp stringifies object when Symbol.match is null", () => {
  const obj = { source: "abc", flags: "", [Symbol.match]: null };
  const r = new RegExp(obj);
  expect(r.source).toBe("[object Object]");
});

test("new RegExp stringifies object when Symbol.match is 0", () => {
  const obj = { source: "abc", flags: "", [Symbol.match]: 0 };
  const r = new RegExp(obj);
  expect(r.source).toBe("[object Object]");
});

test("new RegExp stringifies object when Symbol.match is empty string", () => {
  const obj = { source: "abc", flags: "", [Symbol.match]: "" };
  const r = new RegExp(obj);
  expect(r.source).toBe("[object Object]");
});

test("plain object with source and flags is not a RegExp instance", () => {
  const fake = { source: "abc", flags: "g" };
  expect(() => { RegExp.prototype.exec.call(fake, "abc"); }).toThrow(TypeError);
  expect(() => { RegExp.prototype.test.call(fake, "abc"); }).toThrow(TypeError);
});
