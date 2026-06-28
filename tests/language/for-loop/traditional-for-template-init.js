/*---
description: Traditional for-loop headers whose parts contain template literals with substitutions
features: [compat-traditional-for-loop, template-literals, template-interpolation]
---*/

describe("traditional for-loop header with template literals", () => {
  test("template literal in the init is recognized as a traditional for-loop", () => {
    let out = "";
    for (let s = `v${1}w`; s.length > 0; s = s.slice(1)) {
      out = out + s[0];
    }
    expect(out).toBe("v1w");
  });

  test("template literal with division in the init", () => {
    const seen = [];
    for (let s = `c=${6 / 2}`; seen.length < 1; seen.push(s)) {
      // body runs once, then update pushes the substituted value
    }
    expect(seen).toEqual(["c=3"]);
  });

  test("for-of over template literals is not misread as a traditional header", () => {
    const out = [];
    for (const ch of [`a${1}`, `b${2}`]) {
      out.push(ch);
    }
    expect(out).toEqual(["a1", "b2"]);
  });
});
