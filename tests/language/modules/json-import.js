import { name, version, debug, maxRetries, tags, database } from "./helpers/config.json";
import { count, pi, empty, active, greeting } from "./helpers/simple-values.json";

describe("JSON import", () => {
  test("string values", () => {
    expect(name).toBe("goccia-test");
    expect(version).toBe("1.0.0");
  });

  test("boolean value", () => {
    expect(debug).toBe(true);
  });

  test("number value", () => {
    expect(maxRetries).toBe(3);
  });

  test("array value", () => {
    expect(tags[0]).toBe("alpha");
    expect(tags[1]).toBe("beta");
    expect(tags.length).toBe(2);
  });

  test("nested object value", () => {
    expect(database.host).toBe("localhost");
    expect(database.port).toBe(5432);
  });

  test("null value", () => {
    expect(empty).toBe(null);
  });

  test("false boolean", () => {
    expect(active).toBe(false);
  });

  test("integer value", () => {
    expect(count).toBe(42);
  });

  test("float value", () => {
    expect(pi).toBe(3.14159);
  });

  test("string with spaces", () => {
    expect(greeting).toBe("hello world");
  });
});
