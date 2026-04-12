/*---
description: Tagged template literals
features: [tagged-templates]
---*/

describe("tagged templates", () => {
  test("tag function receives template strings array and substitutions", () => {
    const results = [];
    const tag = (strings, ...values) => {
      results.push(strings);
      results.push(values);
      return "tagged";
    };

    const result = tag`hello ${"world"} and ${42}`;
    expect(result).toBe("tagged");
    expect(results[0].length).toBe(3);
    expect(results[0][0]).toBe("hello ");
    expect(results[0][1]).toBe(" and ");
    expect(results[0][2]).toBe("");
    expect(results[1].length).toBe(2);
    expect(results[1][0]).toBe("world");
    expect(results[1][1]).toBe(42);
  });

  test("template object has raw property", () => {
    let templateObj;
    const tag = (strings) => {
      templateObj = strings;
    };

    tag`hello\nworld`;
    expect(templateObj[0]).toBe("hello\nworld");
    expect(templateObj.raw[0]).toBe("hello\\nworld");
  });

  test("template object is frozen", () => {
    let templateObj;
    const tag = (strings) => {
      templateObj = strings;
      return strings;
    };

    tag`test`;
    expect(Object.isFrozen(templateObj)).toBe(true);
  });

  test("raw property array is frozen", () => {
    let templateObj;
    const tag = (strings) => {
      templateObj = strings;
    };

    tag`test`;
    expect(Object.isFrozen(templateObj.raw)).toBe(true);
  });

  test("raw property is non-enumerable", () => {
    // ES2026 §13.2.8.3 step 8: raw must be non-enumerable, non-writable, non-configurable
    let templateObj;
    const tag = (strings) => {
      templateObj = strings;
    };

    tag`hello`;
    expect(Object.keys(templateObj)).not.toContain("raw");
    const desc = Object.getOwnPropertyDescriptor(templateObj, "raw");
    expect(desc.enumerable).toBe(false);
    expect(desc.writable).toBe(false);
    expect(desc.configurable).toBe(false);
  });

  test("template without interpolations", () => {
    const tag = (strings) => strings[0];
    expect(tag`hello world`).toBe("hello world");
  });

  test("empty template literal", () => {
    const tag = (strings) => strings[0];
    expect(tag``).toBe("");
  });

  test("tag function returns non-string value", () => {
    const tag = () => 42;
    expect(tag`anything`).toBe(42);
  });

  test("tag function returns array", () => {
    const tag = (strings, ...values) => [...strings, ...values];
    const result = tag`a${1}b${2}c`;
    expect(result.length).toBe(5);
    expect(result[0]).toBe("a");
    expect(result[1]).toBe("b");
    expect(result[2]).toBe("c");
    expect(result[3]).toBe(1);
    expect(result[4]).toBe(2);
  });

  test("raw strings preserve escape sequences", () => {
    let raw;
    const tag = (strings) => {
      raw = strings.raw;
    };

    tag`\t\n\r\\`;
    expect(raw[0]).toBe("\\t\\n\\r\\\\");
  });

  test("raw strings with interpolations", () => {
    let raw;
    const tag = (strings) => {
      raw = strings.raw;
    };

    tag`\thello${1}\nworld`;
    expect(raw[0]).toBe("\\thello");
    expect(raw[1]).toBe("\\nworld");
  });

  test("tag as arrow function", () => {
    const tag = (strings, ...values) =>
      strings.reduce((acc, str, i) => acc + str + (values[i] !== undefined ? values[i] : ""), "");

    expect(tag`hello ${"world"}`).toBe("hello world");
  });

  test("method as tag", () => {
    const obj = {
      prefix: ">>",
      tag(strings, ...values) {
        return this.prefix + strings.join("") + values.join(",");
      }
    };

    expect(obj.tag`hello ${1}world`).toBe(">>hello world1");
  });

  test("string parts count is always one more than substitutions", () => {
    let stringsCount;
    let valuesCount;
    const tag = (strings, ...values) => {
      stringsCount = strings.length;
      valuesCount = values.length;
    };

    tag``;
    expect(stringsCount).toBe(1);
    expect(valuesCount).toBe(0);

    tag`a${1}`;
    expect(stringsCount).toBe(2);
    expect(valuesCount).toBe(1);

    tag`a${1}b${2}c${3}`;
    expect(stringsCount).toBe(4);
    expect(valuesCount).toBe(3);
  });

  test("expressions are evaluated in order", () => {
    const order = [];
    const tag = (strings, ...values) => values;
    const push = (v) => { order.push(v); return v; };

    tag`${push(1)}${push(2)}${push(3)}`;
    expect(order.length).toBe(3);
    expect(order[0]).toBe(1);
    expect(order[1]).toBe(2);
    expect(order[2]).toBe(3);
  });

  test("cooked and raw differ for escape sequences", () => {
    let cooked;
    let raw;
    const tag = (strings) => {
      cooked = strings[0];
      raw = strings.raw[0];
    };

    tag`line1\nline2\ttab`;
    expect(cooked).toBe("line1\nline2\ttab");
    expect(raw).toBe("line1\\nline2\\ttab");
  });

  test("backtick and dollar sign escapes in raw", () => {
    let raw;
    const tag = (strings) => {
      raw = strings.raw;
    };

    tag`\`\$`;
    expect(raw[0]).toBe("\\`\\$");
  });

  test("escaped dollar-brace is not an interpolation boundary", () => {
    const x = 42;
    let strings;
    let values;
    const tag = (s, ...v) => { strings = s; values = v; };
    tag`\${x}`;
    expect(strings.length).toBe(1);
    expect(strings[0]).toBe("${x}");
    expect(values.length).toBe(0);
    expect(strings.raw[0]).toBe("\\${x}");
  });

  test("escaped backslash before real interpolation", () => {
    const x = 42;
    let strings;
    let values;
    const tag = (s, ...v) => { strings = s; values = v; };
    tag`\\${x}`;
    expect(strings.length).toBe(2);
    expect(strings[0]).toBe("\\");
    expect(strings[1]).toBe("");
    expect(values.length).toBe(1);
    expect(values[0]).toBe(42);
    expect(strings.raw[0]).toBe("\\\\");
    expect(strings.raw[1]).toBe("");
  });

  test("mixed escaped and real interpolations", () => {
    const a = 1;
    const b = 2;
    let strings;
    let values;
    const tag = (s, ...v) => { strings = s; values = v; };
    tag`\${a} ${b}`;
    expect(strings.length).toBe(2);
    expect(strings[0]).toBe("${a} ");
    expect(strings[1]).toBe("");
    expect(values.length).toBe(1);
    expect(values[0]).toBe(2);
  });

  test("escaped dollar without brace is literal", () => {
    let strings;
    const tag = (s) => { strings = s; };
    tag`price: \$5`;
    expect(strings.length).toBe(1);
    expect(strings[0]).toBe("price: $5");
    expect(strings.raw[0]).toBe("price: \\$5");
  });

  test("triple backslash before dollar-brace is escaped backslash plus escaped dollar", () => {
    const x = 42;
    let strings;
    let values;
    const tag = (s, ...v) => { strings = s; values = v; };
    tag`\\\${x}`;
    expect(strings.length).toBe(1);
    expect(strings[0]).toBe("\\${x}");
    expect(values.length).toBe(0);
  });

  test("same call site returns the identical template object on every invocation (ES2026 §13.2.8.3)", () => {
    // The spec requires GetTemplateObject to return the same frozen object for
    // repeated evaluations of the same Parse Node (call site).
    const tag = (strings) => strings;
    const callSite = () => tag`hello`;
    const a = callSite();
    const b = callSite();
    expect(a).toBe(b);
  });

  test("template object identity is preserved with substitutions", () => {
    const tag = (strings) => strings;
    let x = 1;
    const callSite = () => tag`hello ${x} world`;
    const a = callSite();
    x = 2;
    const b = callSite();
    // The template object (strings array) must be identical; only the substitution differs
    expect(a).toBe(b);
  });

  test("different call sites produce different template objects even with identical content", () => {
    const tag = (strings) => strings;
    // Two syntactically distinct tagged template expressions are two separate
    // Parse Nodes and therefore map to separate entries in [[TemplateMap]].
    const a = tag`hello`;
    const b = tag`hello`;
    expect(a).not.toBe(b);
  });
});
