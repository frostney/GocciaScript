const topLevelThis = this;
const metaUrl = import.meta.url;

describe(".mjs entry source type", () => {
  test(".mjs entries default to module source", () => {
    expect(topLevelThis).toBeUndefined();
  });

  test("import.meta resolves for .mjs entries without source-type config", () => {
    expect(typeof metaUrl).toBe("string");
    expect(metaUrl.endsWith("/mjs-entry-source-type.mjs")).toBe(true);
  });
});
