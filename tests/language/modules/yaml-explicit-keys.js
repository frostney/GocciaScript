import { name, tags, version } from "./helpers/explicit-keys.yaml";

describe("YAML import with explicit keys", () => {
  test("imports top-level explicit scalar keys", () => {
    expect(name).toBe("app");
    expect(version).toBe(1);
    expect(tags.length).toBe(2);
    expect(tags[0]).toBe("alpha");
  });
});
