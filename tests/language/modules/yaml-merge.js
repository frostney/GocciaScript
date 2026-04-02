import { database } from "./helpers/merged.yaml";

describe("YAML import with merge keys", () => {
  test("merged mappings become named exports", () => {
    expect(database.host).toBe("localhost");
    expect(database.port).toBe(7000);
    expect(database.enabled).toBe(true);
  });
});
