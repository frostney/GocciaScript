/*---
description: >
  A raw U+FFFE or U+FFFF in UTF-8 source text is that code unit in a string or
  template literal (ES2026 11.1: SourceCharacter is any Unicode code point).
  This file holds both noncharacters as raw bytes, not as escapes.
features: [string-literals, template-literals]
---*/

describe("raw noncharacters in source text", () => {
  test("U+FFFF in a string literal", () => {
    expect("￿".charCodeAt(0)).toBe(0xffff);
    expect("￿").toBe(String.fromCharCode(0xffff));
  });

  test("U+FFFE in a string literal", () => {
    expect("￾".charCodeAt(0)).toBe(0xfffe);
  });

  test("U+FFFF and U+FFFE in a template literal, beside other characters", () => {
    const text = `a￿\u00fc￾b`;
    expect(text.length).toBe(5);
    expect(text.charCodeAt(1)).toBe(0xffff);
    expect(text.charCodeAt(2)).toBe(0xfc);
    expect(text.charCodeAt(3)).toBe(0xfffe);
  });
});
