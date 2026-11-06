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
});
