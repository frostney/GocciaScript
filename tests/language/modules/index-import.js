import { indexValue, indexDouble } from "./helpers/indexed-module";
import { source } from "./helpers/multi-index";

describe("index file resolution", () => {
  test("import from directory resolves to index.js", () => {
    expect(indexValue).toBe("from-index");
  });

  test("import function from index module", () => {
    expect(indexDouble(21)).toBe(42);
  });

  test("prefers .js over .ts when both index files exist", () => {
    expect(source).toBe("js");
  });
});
