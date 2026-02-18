import { name, version } from "./helpers/json-re-exporter.js";

describe("JSON re-export through JS module", () => {
  test("re-exported string values from JSON", () => {
    expect(name).toBe("goccia-test");
    expect(version).toBe("1.0.0");
  });
});
