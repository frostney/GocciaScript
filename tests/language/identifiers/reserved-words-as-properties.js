/*---
description: Reserved words as property names and identifier edge cases
features: [object-properties, reserved-words, unicode-identifiers]
---*/

test("reserved words as object property names", () => {
  // Reserved words can be used as property names in object literals
  const obj = {
    if: "conditional",
    else: "alternative",
    const: "constant",
    let: "variable",
    class: "constructor",
    extends: "inheritance",
    static: "class-level",
    private: "access-modifier",
    public: "visibility",
    return: "exit",
    break: "loop-exit",
    continue: "loop-next",
  };

  expect(obj.if).toBe("conditional");
  expect(obj.else).toBe("alternative");
  expect(obj.const).toBe("constant");
  expect(obj.let).toBe("variable");
  expect(obj.class).toBe("constructor");
  expect(obj.extends).toBe("inheritance");
  expect(obj.static).toBe("class-level");
  expect(obj.private).toBe("access-modifier");
  expect(obj.public).toBe("visibility");
  expect(obj.return).toBe("exit");
  expect(obj.break).toBe("loop-exit");
  expect(obj.continue).toBe("loop-next");
});

test("reserved words as object property names with bracket notation", () => {
  const obj = {};

  // Setting properties using bracket notation
  obj["if"] = "conditional access";
  obj["class"] = "type definition";
  obj["function"] = "callable"; // Note: function keyword not allowed in GocciaScript
  obj["new"] = "instantiation";
  obj["this"] = "context";
  obj["super"] = "parent";
  obj["import"] = "module";
  obj["export"] = "expose";

  expect(obj["if"]).toBe("conditional access");
  expect(obj["class"]).toBe("type definition");
  expect(obj["function"]).toBe("callable");
  expect(obj["new"]).toBe("instantiation");
  expect(obj["this"]).toBe("context");
  expect(obj["super"]).toBe("parent");
  expect(obj["import"]).toBe("module");
  expect(obj["export"]).toBe("expose");
});

test("reserved words in computed property names", () => {
  const keywords = ["if", "else", "const", "let", "class"];
  const values = [
    "conditional",
    "alternative",
    "constant",
    "variable",
    "constructor",
  ];

  const obj = {};
  keywords.forEach((keyword, index) => {
    obj[keyword] = values[index];
  });

  expect(obj.if).toBe("conditional");
  expect(obj.else).toBe("alternative");
  expect(obj.const).toBe("constant");
  expect(obj.let).toBe("variable");
  expect(obj.class).toBe("constructor");
});

test("special characters in property names", () => {
  const obj = {
    "special-key": "hyphenated",
    "space key": "with spaces",
    "123numeric": "starts with number",
    "": "empty string key",
    "@symbol": "at symbol",
    "#hash": "hash symbol",
    $dollar: "dollar symbol",
    "key.with.dots": "dotted key",
    "key[with]brackets": "bracketed key",
  };

  expect(obj["special-key"]).toBe("hyphenated");
  expect(obj["space key"]).toBe("with spaces");
  expect(obj["123numeric"]).toBe("starts with number");
  expect(obj[""]).toBe("empty string key");
  expect(obj["@symbol"]).toBe("at symbol");
  expect(obj["#hash"]).toBe("hash symbol");
  expect(obj["$dollar"]).toBe("dollar symbol");
  expect(obj["key.with.dots"]).toBe("dotted key");
  expect(obj["key[with]brackets"]).toBe("bracketed key");
});

test("numeric property names and string coercion", () => {
  const obj = {
    1: "one",
    2.5: "two point five",
    0: "zero",
    42: "answer",
    1e3: "exponential",
    0x10: "hexadecimal", // 16 in decimal
    0b1010: "binary", // 10 in decimal
    0o12: "octal", // 10 in decimal
  };

  // Numeric properties are converted to strings
  expect(obj[1]).toBe("one");
  expect(obj["1"]).toBe("one");
  expect(obj[2.5]).toBe("two point five");
  expect(obj["2.5"]).toBe("two point five");
  expect(obj[0]).toBe("zero");
  expect(obj["0"]).toBe("zero");
  expect(obj[42]).toBe("answer");
  expect(obj["42"]).toBe("answer");
  expect(obj[1000]).toBe("exponential");
  expect(obj["1000"]).toBe("exponential");
  expect(obj[16]).toBe("hexadecimal");
  expect(obj["16"]).toBe("hexadecimal");
  expect(obj[10]).toBe("octal"); // Both binary and octal evaluate to 10
  expect(obj["10"]).toBe("octal");
});

test("unicode property names", () => {
  const obj = {
    café: "coffee",
    naïve: "innocent",
    resumé: "curriculum vitae",
    "🌟": "star emoji",
    αβγ: "greek letters",
    测试: "chinese test",
    "\u0041": "unicode A",
    "\u{1F4A9}": "pile of poo emoji",
  };

  expect(obj.café).toBe("coffee");
  expect(obj.naïve).toBe("innocent");
  expect(obj.resumé).toBe("curriculum vitae");
  expect(obj["café"]).toBe("coffee");
  expect(obj["naïve"]).toBe("innocent");
  expect(obj["resumé"]).toBe("curriculum vitae");
  expect(obj["🌟"]).toBe("star emoji");
  expect(obj["αβγ"]).toBe("greek letters");
  expect(obj["测试"]).toBe("chinese test");
  expect(obj["A"]).toBe("unicode A"); // \u0041 is 'A'
  expect(obj["💩"]).toBe("pile of poo emoji"); // \u{1F4A9} is 💩
});

test("property name evaluation order with side effects", () => {
  let evalOrder = [];

  const getKey = (name) => {
    evalOrder.push(`key-${name}`);
    return name;
  };

  const getValue = (name) => {
    evalOrder.push(`value-${name}`);
    return `${name}-value`;
  };

  const obj = {
    [getKey("first")]: getValue("first"),
    regularKey: getValue("regular"),
    [getKey("second")]: getValue("second"),
    [getKey("third")]: getValue("third"),
  };

  expect(obj.first).toBe("first-value");
  expect(obj.regularKey).toBe("regular-value");
  expect(obj.second).toBe("second-value");
  expect(obj.third).toBe("third-value");

  // Keys should be evaluated first, then values
  expect(evalOrder).toEqual([
    "key-first",
    "value-first",
    "value-regular",
    "key-second",
    "value-second",
    "key-third",
    "value-third",
  ]);
});

test("property names with type coercion", () => {
  const obj = {};

  // Various types as property names
  obj[true] = "boolean true";
  obj[false] = "boolean false";
  obj[null] = "null value";
  obj[undefined] = "undefined value";
  obj[Symbol.for("test")] = "symbol value";

  // Access with original types
  expect(obj[true]).toBe("boolean true");
  expect(obj[false]).toBe("boolean false");
  expect(obj[null]).toBe("null value");
  expect(obj[undefined]).toBe("undefined value");
  expect(obj[Symbol.for("test")]).toBe("symbol value");

  // Access with string representations
  expect(obj["true"]).toBe("boolean true");
  expect(obj["false"]).toBe("boolean false");
  expect(obj["null"]).toBe("null value");
  expect(obj["undefined"]).toBe("undefined value");

  // Symbols don't get string coercion
  expect(obj["Symbol(test)"]).toBeUndefined();
});

test("reserved words in method names", () => {
  const obj = {
    if() {
      return "if method";
    },
    else() {
      return "else method";
    },
    class() {
      return "class method";
    },
    const() {
      return "const method";
    },
    let() {
      return "let method";
    },
    static() {
      return "static method";
    },
    super() {
      return "super method";
    },
    this() {
      return "this method";
    },
    return() {
      return "return method";
    },
  };

  expect(obj.if()).toBe("if method");
  expect(obj.else()).toBe("else method");
  expect(obj.class()).toBe("class method");
  expect(obj.const()).toBe("const method");
  expect(obj.let()).toBe("let method");
  expect(obj.static()).toBe("static method");
  expect(obj.super()).toBe("super method");
  expect(obj.this()).toBe("this method");
  expect(obj.return()).toBe("return method");
});
