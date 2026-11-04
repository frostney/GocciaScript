/*---
description: Blob constructor (File API §3.1)
features: [Blob]
---*/

describe("Blob constructor", () => {
  test("with no arguments is empty with an empty type", () => {
    const b = new Blob();
    expect(b instanceof Blob).toBe(true);
    expect(b.size).toBe(0);
    expect(b.type).toBe("");
  });

  test("Blob.length is 0 and Blob.name is Blob", () => {
    expect(Blob.length).toBe(0);
    expect(Blob.name).toBe("Blob");
  });

  test("cannot be called without new", () => {
    expect(() => Blob()).toThrow(TypeError);
  });

  test("encodes string parts as UTF-8", async () => {
    const b = new Blob(["héllo", "\u{1F600}"]);
    expect(b.size).toBe(6 + 4);
    expect(await b.text()).toBe("héllo\u{1F600}");
  });

  test("replaces lone surrogates in strings with U+FFFD", async () => {
    const b = new Blob(["\uD800", "x\uDC00"]);
    expect(b.size).toBe(3 + 1 + 3);
    expect(await b.text()).toBe("�x�");
  });

  test("converts other values to strings", async () => {
    const b = new Blob([1, null, { toString: () => "o" }]);
    expect(await b.text()).toBe("1nullo");
  });

  test("copies ArrayBuffer, typed array and DataView parts", async () => {
    const buffer = new Uint8Array([1, 2, 3, 4]).buffer;
    const b = new Blob([buffer, new Uint8Array(buffer, 1, 2), new DataView(buffer, 3, 1), new Uint16Array([0x0605])]);
    expect(Array.from(await b.bytes())).toEqual([1, 2, 3, 4, 2, 3, 4, 5, 6]);
  });

  test("copies Blob parts and ignores their type", async () => {
    const inner = new Blob(["ab"], { type: "text/plain" });
    const b = new Blob([inner, "c", inner]);
    expect(await b.text()).toBe("abcab");
    expect(b.type).toBe("");
  });

  test("accepts any iterable of parts", async () => {
    const parts = new Set(["a", "b"]);
    expect(await new Blob(parts).text()).toBe("ab");
    const gen = { *[Symbol.iterator]() {
      yield "x";
      yield new Uint8Array([0x79]);
    } };
    expect(await new Blob(gen).text()).toBe("xy");
  });

  test("copies buffer bytes after every part has converted", async () => {
    const bytes = new Uint8Array([65]);
    const b = new Blob([bytes, { toString: () => { bytes[0] = 66; return "!"; } }]);
    expect(await b.text()).toBe("B!");
  });

  test("a detached buffer contributes no bytes", async () => {
    const buffer = new ArrayBuffer(4);
    buffer.transfer();
    expect(new Blob([buffer, "x"]).size).toBe(1);
  });

  test("refuses blobParts that are not an iterable object", () => {
    expect(() => new Blob("abc")).toThrow(TypeError);
    expect(() => new Blob(null)).toThrow(TypeError);
    expect(() => new Blob(5)).toThrow(TypeError);
    expect(() => new Blob({})).toThrow(TypeError);
  });

  test("undefined blobParts is no parts", () => {
    expect(new Blob(undefined, { type: "a/b" }).size).toBe(0);
  });

  test("refuses a SharedArrayBuffer and views of one", () => {
    const shared = new SharedArrayBuffer(4);
    expect(() => new Blob([shared])).toThrow(TypeError);
    expect(() => new Blob([new Uint8Array(shared)])).toThrow(TypeError);
  });

  test("refuses a resizable ArrayBuffer and views of one", () => {
    const resizable = new ArrayBuffer(4, { maxByteLength: 8 });
    expect(() => new Blob([resizable])).toThrow(TypeError);
    expect(() => new Blob([new Uint8Array(resizable)])).toThrow(TypeError);
  });

  test("a Symbol part is a TypeError", () => {
    expect(() => new Blob([Symbol("s")])).toThrow(TypeError);
  });

  test("a part whose toString throws propagates the error", () => {
    const err = new Error("boom");
    expect(() => new Blob([{ toString() { throw err; } }])).toThrow(Error);
  });
});

describe("Blob options", () => {
  test("type is lower-cased", () => {
    expect(new Blob([], { type: "Text/HTML;Charset=UTF-8" }).type).toBe("text/html;charset=utf-8");
  });

  test("a type with a character outside U+0020-U+007E is the empty string", () => {
    expect(new Blob([], { type: "text/pläin" }).type).toBe("");
    expect(new Blob([], { type: "text/\nplain" }).type).toBe("");
  });

  test("undefined and null options are the defaults", () => {
    expect(new Blob(["a"], undefined).type).toBe("");
    expect(new Blob(["a"], null).type).toBe("");
  });

  test("options that are not an object are a TypeError", () => {
    expect(() => new Blob([], 5)).toThrow(TypeError);
    expect(() => new Blob([], "text/plain")).toThrow(TypeError);
  });

  test("endings must be transparent or native", () => {
    expect(() => new Blob([], { endings: "x" })).toThrow(TypeError);
    expect(() => new Blob([], { endings: "Native" })).toThrow(TypeError);
  });

  test("transparent endings keep line endings", async () => {
    expect(await new Blob(["a\r\nb\rc\nd"], { endings: "transparent" }).text()).toBe("a\r\nb\rc\nd");
  });

  test("native endings convert CR LF, CR and LF to the platform's line ending", async () => {
    const text = await new Blob(["a\r\nb\rc\nd"], { endings: "native" }).text();
    expect(text === "a\nb\nc\nd" || text === "a\r\nb\r\nc\r\nd").toBe(true);
  });

  test("native endings apply to strings only", async () => {
    const b = new Blob([new Uint8Array([13]), "\r"], { endings: "native" });
    const bytes = Array.from(await b.bytes());
    expect(bytes[0]).toBe(13);
    expect(bytes.length === 2 || bytes.length === 3).toBe(true);
  });

  test("members are read in lexicographic order, endings before type", () => {
    const order = [];
    new Blob([], {
      get type() { order.push("type"); return ""; },
      get endings() { order.push("endings"); return "transparent"; },
    });
    expect(order).toEqual(["endings", "type"]);
  });

  test("blobParts convert before options", () => {
    const order = [];
    new Blob([{ toString() { order.push("part"); return ""; } }], {
      get endings() { order.push("endings"); return undefined; },
    });
    expect(order).toEqual(["part", "endings"]);
  });
});

describe("Blob subclassing", () => {
  test("a subclass instance is a Blob with the subclass prototype", async () => {
    class Named extends Blob {
      describe() {
        return `${this.size} ${this.type}`;
      }
    }
    const n = new Named(["xyz"], { type: "a/b" });
    expect(n instanceof Named).toBe(true);
    expect(n instanceof Blob).toBe(true);
    expect(n.describe()).toBe("3 a/b");
    expect(await n.text()).toBe("xyz");
  });
});
