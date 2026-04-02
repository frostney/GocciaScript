import { name, version } from "./helpers/yaml-re-exporter.js";

describe("YAML re-export through JS module", () => {
  test("re-exported string values from YAML", () => {
    expect(name).toBe("goccia-test");
    expect(version).toBe("1.0.0");
  });
});
