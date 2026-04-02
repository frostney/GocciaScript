import { count, created, custom, enabled, name, payload, typed } from "./helpers/tagged-values.yaml";

describe("YAML import with tags and directives", () => {
  test("imports tagged values with standard coercions", () => {
    expect(name).toBe("true");
    expect(count).toBe(42);
    expect(enabled).toBe(true);
    expect(custom.title).toBe("module");
    expect(custom.tagName).toBe("tag:example.com,2026:widget");
    expect(typed[0]).toBe("123");
    expect(typed[1]).toBe(5);
    expect(created.tagName).toBe("tag:yaml.org,2002:timestamp");
    expect(created.value).toBe("2026-04-02T10:30:00");
    expect(payload.tagName).toBe("tag:yaml.org,2002:binary");
    expect(payload.value).toBe("Hello");
  });
});
