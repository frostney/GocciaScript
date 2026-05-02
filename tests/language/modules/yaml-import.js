import { name, version, debug, maxRetries, tags, database } from "./helpers/config.yaml";
import { count, pi, empty, active, greeting } from "./helpers/simple-values.yml";
import { "default" as scalarDefault } from "./helpers/scalar.yaml";
import * as unicodeConfig from "./helpers/unicode-config.yaml";

describe("YAML import", () => {
  test("string values", () => {
    expect(name).toBe("goccia-test");
    expect(version).toBe("1.0.0");
  });

  test("boolean value", () => {
    expect(debug).toBe(true);
  });

  test("number value", () => {
    expect(maxRetries).toBe(3);
  });

  test("array value", () => {
    expect(tags[0]).toBe("alpha");
    expect(tags[1]).toBe("beta");
    expect(tags.length).toBe(2);
  });

  test("nested object value", () => {
    expect(database.host).toBe("localhost");
    expect(database.port).toBe(5432);
  });

  test("null value", () => {
    expect(empty).toBe(null);
  });

  test("false boolean", () => {
    expect(active).toBe(false);
  });

  test("integer value", () => {
    expect(count).toBe(42);
  });

  test("float value", () => {
    expect(pi).toBe(3.14159);
  });

  test("string with spaces", () => {
    expect(greeting).toBe("hello world");
  });

  test("single scalar document default export", () => {
    expect(scalarDefault).toBe(42);
  });

  test("preserves UTF-8 text in file-backed YAML modules", () => {
    expect(unicodeConfig.name).toBe("Jos\u00e9");
    expect(unicodeConfig["d\u00e9j\u00e0"]).toBe("vu");
    expect(unicodeConfig.nested.city).toBe("Z\u00fcrich");
  });
});
