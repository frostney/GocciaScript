import {
  name,
  version,
  debug,
  maxRetries,
  tags,
  database,
} from "./helpers/config.json5";

describe("JSON5 import", () => {
  test("exports top-level scalar values", () => {
    expect(name).toBe("goccia-test");
    expect(version).toBe("1.0.0");
    expect(debug).toBe(true);
    expect(maxRetries).toBe(3);
  });

  test("exports arrays and nested objects", () => {
    expect(tags.length).toBe(2);
    expect(tags[0]).toBe("alpha");
    expect(database.host).toBe("localhost");
    expect(database.port).toBe(5432);
  });

  test("JSON5.parse reports malformed JSON5 syntax", () => {
    expect(() => JSON5.parse("{a: 0x}")).toThrow(SyntaxError);
  });
});
