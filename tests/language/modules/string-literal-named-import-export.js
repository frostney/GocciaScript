import {
  "0" as firstDoc,
  "foo-bar" as fooBar,
  "default" as defaultValue,
} from "./helpers/string-literal-export-source.js";
import {
  firstDoc as reExportedFirstDoc,
  "foo-baz" as fooBaz,
  defaultValue as reExportedDefault,
} from "./helpers/string-literal-re-exporter.js";
import {
  "0" as jsonZero,
  "foo-bar" as jsonFooBar,
  "default" as jsonDefault,
} from "./helpers/string-literal-keys.json";
import {
  "0" as yamlZero,
  "foo-bar" as yamlFooBar,
  "default" as yamlDefault,
} from "./helpers/string-literal-keys.yaml";

describe("string-literal named imports and exports", () => {
  test("imports string-literal export names from JavaScript modules", () => {
    expect(firstDoc).toBe("first document");
    expect(fooBar).toBe(42);
    expect(defaultValue).toBe("fallback");
  });

  test("re-exports string-literal source and exported names", () => {
    expect(reExportedFirstDoc).toBe("first document");
    expect(fooBaz).toBe(42);
    expect(reExportedDefault).toBe("fallback");
  });

  test("imports string-literal names from JSON modules", () => {
    expect(jsonZero).toBe("json-zero");
    expect(jsonFooBar).toBe("json-hyphen");
    expect(jsonDefault).toBe("json-default");
  });

  test("imports string-literal names from YAML modules", () => {
    expect(yamlZero).toBe("yaml-zero");
    expect(yamlFooBar).toBe("yaml-hyphen");
    expect(yamlDefault).toBe("yaml-default");
  });
});
