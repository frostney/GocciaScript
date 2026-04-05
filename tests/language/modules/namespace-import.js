/*---
description: >
  Namespace imports expose a frozen module namespace object for script modules
  and structured-data modules.
features: [modules, namespace-imports]
---*/

import * as math from "./helpers/math-utils.js";
import * as mathAgain from "./helpers/math-utils.js";
import * as configJson from "./helpers/config.json";
import * as configJsonAgain from "./helpers/config.json";
import * as configToml from "./helpers/config.toml";
import * as configYaml from "./helpers/config.yaml";

describe("namespace import", () => {
  test("creates a frozen null-prototype namespace for JavaScript modules", () => {
    expect(math.add(2, 3)).toBe(5);
    expect(math.multiply(4, 5)).toBe(20);
    expect(math.PI).toBe(3.14159);
    expect(Object.keys(math)).toEqual(["add", "multiply", "PI"]);
    expect(Object.getPrototypeOf(math)).toBeNull();
    expect(Object.isFrozen(math)).toBe(true);
  });

  test("reuses the same namespace object for repeated imports", () => {
    expect(mathAgain).toBe(math);
    expect(configJsonAgain).toBe(configJson);
  });

  test("namespace exports remain read-only", () => {
    expect(() => {
      math.PI = 0;
    }).toThrow(TypeError);
    expect(math.PI).toBe(3.14159);
  });

  test("creates a namespace object for JSON modules", () => {
    expect(configJson.name).toBe("goccia-test");
    expect(configJson.version).toBe("1.0.0");
    expect(configJson.database.host).toBe("localhost");
    expect(configJson.database.port).toBe(5432);
    expect(Object.keys(configJson)).toEqual([
      "name",
      "version",
      "debug",
      "maxRetries",
      "tags",
      "database",
    ]);
  });

  test("creates a namespace object for YAML modules", () => {
    expect(configYaml.name).toBe("goccia-test");
    expect(configYaml.version).toBe("1.0.0");
    expect(configYaml.database.host).toBe("localhost");
    expect(configYaml.database.port).toBe(5432);
    expect(Object.keys(configYaml)).toEqual([
      "name",
      "version",
      "debug",
      "maxRetries",
      "tags",
      "database",
    ]);
  });

  test("creates a namespace object for TOML modules", () => {
    expect(configToml.name).toBe("goccia-test");
    expect(configToml.version).toBe("1.0.0");
    expect(configToml.database.host).toBe("localhost");
    expect(configToml.database.port).toBe(5432);
    expect(configToml.servers.length).toBe(2);
    expect(configToml.servers[1].name).toBe("beta");
    expect(Object.keys(configToml)).toEqual([
      "name",
      "version",
      "debug",
      "maxRetries",
      "release",
      "alarm",
      "tags",
      "database",
      "servers",
    ]);
  });
});
