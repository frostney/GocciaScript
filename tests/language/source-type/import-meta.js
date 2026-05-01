/*---
description: >
  --source-type=module makes the entry script a Module, so import.meta
  resolves to a Module Record's import meta object (ES2026 §13.3.12.1)
  and exposes a `url` string identifying this file.
---*/

const meta = import.meta;
const metaUrl = import.meta.url;

describe("source-type=module entry import.meta", () => {
  test("import.meta is an object", () => {
    expect(typeof meta).toBe("object");
    expect(meta).not.toBeNull();
  });

  test("import.meta.url is a string", () => {
    expect(typeof metaUrl).toBe("string");
  });

  test("import.meta.url points at this file", () => {
    expect(metaUrl.endsWith("/import-meta.js")).toBe(true);
  });
});
