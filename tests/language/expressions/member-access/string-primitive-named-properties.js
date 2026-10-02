/*---
description: |
  A named property read on a string primitive answers as a String object
  created from it would (ES2026 §6.2.5.5 GetValue, §10.4.3 String exotic
  objects): a character for a canonical index inside the string, the length,
  and otherwise whatever String.prototype and its chain hold at that moment,
  with the primitive itself as the receiver.
features: [property-access, String.prototype]
---*/

describe("named property read on a string primitive", () => {
  test("methods come from String.prototype and are shared between strings", () => {
    const first = "bytecode";
    const second = "x";
    expect(typeof first.charCodeAt).toBe("function");
    expect(first.charCodeAt).toBe(second.charCodeAt);
    expect(first.charCodeAt).toBe(String.prototype.charCodeAt);
    expect(first.charCodeAt(1)).toBe(121);
    expect(first.toUpperCase()).toBe("BYTECODE");
    expect(first.constructor).toBe(String);
    expect(first.toString).toBe(String.prototype.toString);
  });

  test("length counts UTF-16 code units", () => {
    expect("".length).toBe(0);
    expect("bytecode".length).toBe(8);
    expect("héllo".length).toBe(5);
    expect("😀".length).toBe(2);
  });

  test("a missing name is undefined", () => {
    const text = "abc";
    expect(text.nope).toBe(undefined);
    expect(text.Length).toBe(undefined);
  });

  test("a method added to String.prototype is visible at once and gone once deleted", () => {
    const text = "abc";
    expect(text.shout).toBe(undefined);
    const methods = {
      shout() {
        return this.toUpperCase() + "!";
      },
    };
    String.prototype.shout = methods.shout;
    try {
      expect(text.shout()).toBe("ABC!");
      expect("".shout()).toBe("!");
    } finally {
      delete String.prototype.shout;
    }
    expect(text.shout).toBe(undefined);
  });

  test("a replaced method is the one a later read finds", () => {
    const text = "abc";
    const original = String.prototype.trim;
    expect(text.trim()).toBe("abc");
    String.prototype.trim = () => "replaced";
    try {
      expect(text.trim()).toBe("replaced");
    } finally {
      String.prototype.trim = original;
    }
    expect(text.trim()).toBe("abc");
  });

  test("a getter on String.prototype receives the primitive", () => {
    Object.defineProperty(String.prototype, "firstCharacter", {
      get() {
        return this[0];
      },
      configurable: true,
    });
    Object.defineProperty(String.prototype, "receiverType", {
      get() {
        return typeof this;
      },
      configurable: true,
    });
    try {
      expect("bytecode".firstCharacter).toBe("b");
      expect("".firstCharacter).toBe(undefined);
      expect("bytecode".receiverType).toBe("string");
    } finally {
      delete String.prototype.firstCharacter;
      delete String.prototype.receiverType;
    }
  });

  test("a property of Object.prototype is inherited through String.prototype", () => {
    Object.prototype.inheritedThroughChain = "from object";
    try {
      expect("abc".inheritedThroughChain).toBe("from object");
    } finally {
      delete Object.prototype.inheritedThroughChain;
    }
    expect("abc".inheritedThroughChain).toBe(undefined);
  });

  test("the string's own length shadows a length on the prototype chain", () => {
    const text = "abc";
    Object.prototype.length = 99;
    try {
      expect(text.length).toBe(3);
    } finally {
      delete Object.prototype.length;
    }
  });

  test("reading a property of a string in a loop keeps finding the same method", () => {
    const text = "bytecode";
    let total = 0;
    for (const index of [0, 1, 2, 3, 4, 5, 6, 7]) {
      total = total + text.charCodeAt(index);
    }
    expect(total).toBe(847);
  });
});

describe("character indices of a string", () => {
  const text = "abcdefghijk";

  test("only the canonical decimal form of an index names a character", () => {
    expect(text["0"]).toBe("a");
    expect(text["9"]).toBe("j");
    expect(text["10"]).toBe("k");
    expect(text["11"]).toBe(undefined);
    expect(text["00"]).toBe(undefined);
    expect(text["03"]).toBe(undefined);
    expect(text["-0"]).toBe(undefined);
    expect(text["-1"]).toBe(undefined);
    expect(text["+1"]).toBe(undefined);
    expect(text[" 1"]).toBe(undefined);
    expect(text["1 "]).toBe(undefined);
    expect(text["1.0"]).toBe(undefined);
    expect(text["1e0"]).toBe(undefined);
    expect(text["0x1"]).toBe(undefined);
    expect(text["$1"]).toBe(undefined);
    expect(text["１"]).toBe(undefined);
    expect(text[""]).toBe(undefined);
  });

  test("an index beyond the 32-bit range is not a character", () => {
    expect(text["2147483647"]).toBe(undefined);
    expect(text["2147483648"]).toBe(undefined);
    expect(text["4294967296"]).toBe(undefined);
    expect(text["9999999999"]).toBe(undefined);
    expect(text["99999999999"]).toBe(undefined);
    expect(text["18446744073709551616"]).toBe(undefined);
  });

  test("an index outside the string falls through to the prototype chain", () => {
    String.prototype[20] = "from prototype";
    String.prototype["03"] = "not an index";
    try {
      expect(text[20]).toBe("from prototype");
      expect(text["20"]).toBe("from prototype");
      expect(text["03"]).toBe("not an index");
      expect(text[3]).toBe("d");
    } finally {
      delete String.prototype[20];
      delete String.prototype["03"];
    }
    expect(text[20]).toBe(undefined);
  });

  test("a String object reports the same indices as own properties", () => {
    const boxed = Object("ab");
    expect(Object.getOwnPropertyNames(boxed)).toEqual(["0", "1", "length"]);
    expect(Object.keys(boxed)).toEqual(["0", "1"]);
    expect(Object.hasOwn(boxed, "0")).toBe(true);
    expect(Object.hasOwn(boxed, "1")).toBe(true);
    expect(Object.hasOwn(boxed, "2")).toBe(false);
    expect(Object.hasOwn(boxed, "01")).toBe(false);
    expect(Object.hasOwn(boxed, "-1")).toBe(false);
    expect(Object.hasOwn(boxed, "length")).toBe(true);
    expect(Object.getOwnPropertyDescriptor(boxed, "1")).toEqual({
      value: "b",
      writable: false,
      enumerable: true,
      configurable: false,
    });
    expect(Object.getOwnPropertyDescriptor(boxed, "2")).toBe(undefined);
    expect(Object.getOwnPropertyDescriptor(boxed, "01")).toBe(undefined);
    expect(Reflect.deleteProperty(boxed, "0")).toBe(false);
    expect(Reflect.deleteProperty(boxed, "length")).toBe(false);
    expect(Reflect.deleteProperty(boxed, "5")).toBe(true);
    expect(Reflect.deleteProperty(boxed, "01")).toBe(true);
  });

  test("an expando index beyond the string sorts after the characters", () => {
    const boxed = Object("ab");
    boxed[5] = "five";
    boxed.name = "label";
    boxed["03"] = "text key";
    expect(Object.getOwnPropertyNames(boxed)).toEqual(["0", "1", "5", "length", "name", "03"]);
    expect(boxed[5]).toBe("five");
  });
});
