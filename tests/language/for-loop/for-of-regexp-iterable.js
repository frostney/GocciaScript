/*---
description: Assignment-target for-of heads preserve the regular-expression lexical goal
features: [traditional-for-loop]
---*/

describe("for-of regexp iterable with traditional loops enabled", () => {
  test("greater than", () => {
    let x;
    for (x of /a>/.exec("a>")) {}
    expect(x).toBe("a>");
  });

  test("less than", () => {
    let x;
    for (x of /a</.exec("a<")) {}
    expect(x).toBe("a<");
  });

  test("equals", () => {
    let x;
    for (x of /a=/.exec("a=")) {}
    expect(x).toBe("a=");
  });

  test("plus", () => {
    let x;
    for (x of /a+/.exec("aaa")) {}
    expect(x).toBe("aaa");
  });

  test("comma", () => {
    let x;
    for (x of /a,/.exec("a,")) {}
    expect(x).toBe("a,");
  });

  test("leading punctuation", () => {
    let x;
    for (x of />/.exec(">")) {}
    expect(x).toBe(">");
  });

  test("semicolon", () => {
    let x;
    for (x of /a;/.exec("a;")) {}
    expect(x).toBe("a;");
  });

  test("escaped parenthesis", () => {
    let x;
    for (x of /a\)/.exec("a)")) {}
    expect(x).toBe("a)");
  });

  test("parenthesized target", () => {
    let x;
    for ((x) of /a>/.exec("a>")) {}
    expect(x).toBe("a>");
  });

  test("binding named of", () => {
    let of;
    for (of of /a>/.exec("a>")) {}
    expect(of).toBe("a>");
  });

  test("property named of", () => {
    const holder = {};
    for (holder.of of /a>/.exec("a>")) {}
    expect(holder.of).toBe("a>");
  });

  test("prefix increment operand named of keeps its traditional header", () => {
    let x;
    for (x of /a>/.exec("a>")) {}
    let of = 0;
    for (++of; of < 2; of++) {}
    expect(x).toBe("a>");
    expect(of).toBe(2);
  });

  test("prefix decrement operand named of keeps its traditional header", () => {
    let x;
    for (x of /a>/.exec("a>")) {}
    let of = 2;
    for (--of; of > 0; of--) {}
    expect(x).toBe("a>");
    expect(of).toBe(0);
  });

  test("await operand named of keeps its traditional header", async () => {
    let x;
    for (x of /a>/.exec("a>")) {}
    const of = Promise.resolve(2);
    for (await of; false;) {}
    expect(x).toBe("a>");
    expect(await of).toBe(2);
  });

  test("yield operand named of keeps its traditional header", () => {
    let x;
    for (x of /a>/.exec("a>")) {}
    const holder = {
      *values() {
        const of = 2;
        for (yield of; false;) {}
      }
    };
    expect(x).toBe("a>");
    expect(holder.values().next().value).toBe(2);
  });

  test("a nested non-async arrow keeps await as an identifier", async () => {
    let x;
    for (x of /a>/.exec("a>")) {}
    let callback;
    for (callback = () => await / 2; false;) {}
    expect(x).toBe("a>");
    expect(typeof callback).toBe("function");
  });

  test("satisfies identifier", () => {
    let satisfies;
    for (satisfies of /a>/.exec("a>")) {}
    expect(satisfies).toBe("a>");
  });

  test("as identifier", () => {
    let as;
    for (as of /a>/.exec("a>")) {}
    expect(as).toBe("a>");
  });

  test("async member target", () => {
    const async = {};
    for (async.x of /a>/.exec("a>")) {}
    expect(async.x).toBe("a>");
  });

  test("async arrow parameters named of keep their traditional header", () => {
    let x;
    for (x of /a>/.exec("a>")) {}
    for (async of => of; false;) {}
    expect(x).toBe("a>");
  });

  test("an operator keyword used as a member name ends the target", () => {
    const holder = {};
    for (holder.in of /a>/.exec("a>")) {}
    expect(holder.in).toBe("a>");
  });

  test("await used as a script identifier can start a member target", () => {
    const await = {};
    for (await.x of /a>/.exec("a>")) {}
    expect(await.x).toBe("a>");
  });
});
