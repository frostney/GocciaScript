import {
  name,
  version,
  debug,
  maxRetries,
  release,
  alarm,
  tags,
  database,
  servers,
} from "./helpers/config.toml";

describe("TOML import", () => {
  test("exports top-level scalar values", () => {
    expect(name).toBe("goccia-test");
    expect(version).toBe("1.0.0");
    expect(debug).toBe(true);
    expect(maxRetries).toBe(3);
    expect(release).toBe("2026-04-04T12:30:45Z");
    expect(alarm).toBe("07:32");
  });

  test("exports arrays and nested tables", () => {
    expect(tags.length).toBe(2);
    expect(tags[0]).toBe("alpha");
    expect(database.host).toBe("localhost");
    expect(database.port).toBe(5432);
  });

  test("exports arrays of tables as arrays of objects", () => {
    expect(servers.length).toBe(2);
    expect(servers[0].name).toBe("alpha");
    expect(servers[1].ip).toBe("10.0.0.2");
  });
});
