import { indexValue, indexDouble } from "./helpers/indexed-module";

describe("index file resolution", () => {
  test("import from directory resolves to index.js", () => {
    expect(indexValue).toBe("from-index");
  });

  test("import function from index module", () => {
    expect(indexDouble(21)).toBe(42);
  });
});
