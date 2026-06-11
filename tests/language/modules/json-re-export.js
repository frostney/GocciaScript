import {
  attributedName,
  attributedVersion,
  configTextDefault,
  configTextModule,
  name,
  version,
} from "./helpers/json-re-exporter.js";

describe("JSON re-export through JS module", () => {
  test("re-exported string values from JSON", () => {
    expect(name).toBe("goccia-test");
    expect(version).toBe("1.0.0");
  });

  test("re-exported import attributes select the requested module kind", () => {
    expect(attributedName).toBe("goccia-test");
    expect(attributedVersion).toBe("1.0.0");
    expect(
      configTextDefault.startsWith("{\n  \"name\": \"goccia-test\""),
    ).toBe(true);
    expect(configTextModule.default).toBe(configTextDefault);
    expect(Object.keys(configTextModule)).toEqual(["default"]);
  });
});
