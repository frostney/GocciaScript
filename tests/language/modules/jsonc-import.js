import {
  name,
  version,
  debug,
  maxRetries,
  tags,
  database,
} from "./helpers/config.jsonc";

describe("JSONC import", () => {
  test("exports top-level scalar values", () => {
    expect(name).toBe("goccia-jsonc");
    expect(version).toBe("2.0.0");
    expect(debug).toBe(false);
    expect(maxRetries).toBe(5);
  });

  test("exports arrays and nested objects", () => {
    expect(tags.length).toBe(2);
    expect(tags[0]).toBe("stable");
    expect(database.host).toBe("db.example.com");
    expect(database.port).toBe(3306);
  });

  test("fails on malformed JSONC", () => {
    expect(() => JSON5.parse('{ "key": }')).toThrow(SyntaxError);
  });
});
