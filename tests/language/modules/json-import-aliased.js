import { name as appName, version as appVersion } from "./helpers/config.json";

describe("JSON import with aliases", () => {
  test("aliased string import", () => {
    expect(appName).toBe("goccia-test");
  });

  test("aliased version import", () => {
    expect(appVersion).toBe("1.0.0");
  });
});
