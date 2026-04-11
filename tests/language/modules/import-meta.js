import { childUrl, childMeta } from "./helpers/import-meta-child.js";
import { getUrl } from "./helpers/import-meta-function.js";

describe("import.meta", () => {
  // On Windows, file URLs include a drive letter (e.g. file:///D:/...).
  // Detect by checking our own URL so absolute-path tests can expect the exact result.
  const driveMatch = import.meta.url.match(/^file:\/\/\/([A-Z]:)/);

  test("import.meta is an object", () => {
    expect(typeof import.meta).toBe("object");
  });

  test("import.meta has null prototype", () => {
    expect(Object.getPrototypeOf(import.meta)).toBe(null);
  });

  test("import.meta.url is a string", () => {
    expect(typeof import.meta.url).toBe("string");
  });

  test("import.meta.url starts with file://", () => {
    expect(import.meta.url.startsWith("file://")).toBe(true);
  });

  test("import.meta.url contains the current file name", () => {
    expect(import.meta.url.includes("import-meta.js")).toBe(true);
  });

  test("import.meta.url is an absolute path", () => {
    // file:// URLs have at least three slashes
    expect(import.meta.url.startsWith("file:///")).toBe(true);
  });

  test("import.meta is identity-stable", () => {
    const a = import.meta;
    const b = import.meta;
    expect(a === b).toBe(true);
  });

  test("import.meta properties are writable", () => {
    // Per spec, import.meta properties are configurable and writable
    import.meta.custom = "test";
    expect(import.meta.custom).toBe("test");
  });

  test("import.meta.resolve is a function", () => {
    expect(typeof import.meta.resolve).toBe("function");
  });

  test("import.meta.resolve returns a file:// URL", () => {
    const resolved = import.meta.resolve("./helpers/math.js");
    expect(typeof resolved).toBe("string");
    expect(resolved.startsWith("file://")).toBe(true);
  });

  test("import.meta.resolve resolves relative paths", () => {
    const resolved = import.meta.resolve("./helpers/math.js");
    expect(resolved.includes("helpers/math.js")).toBe(true);
    // Should not contain ./ in the resolved path
    expect(resolved.includes("./")).toBe(false);
  });

  test("import.meta.resolve resolves parent directory paths", () => {
    const resolved = import.meta.resolve("../modules/helpers/math.js");
    expect(resolved.includes("helpers/math.js")).toBe(true);
  });

  test("import.meta.resolve handles absolute paths", () => {
    const resolved = import.meta.resolve("/absolute/path/file.js");
    // On Windows, ExpandFileName prepends the current drive letter (e.g. D:)
    const expected = driveMatch
      ? "file:///" + driveMatch[1] + "/absolute/path/file.js"
      : "file:///absolute/path/file.js";
    expect(resolved).toBe(expected);
  });

  test("import.meta.resolve percent-encodes special characters", () => {
    const resolved = import.meta.resolve("/path with spaces/file#1.js");
    // On Windows, ExpandFileName prepends the current drive letter (e.g. D:)
    const expected = driveMatch
      ? "file:///" + driveMatch[1] + "/path%20with%20spaces/file%231.js"
      : "file:///path%20with%20spaces/file%231.js";
    expect(resolved).toBe(expected);
  });

  test("import.meta.url percent-encodes spaces in the path", () => {
    // The URL should not contain raw spaces
    expect(import.meta.url.includes(" ")).toBe(false);
  });

  test("import.meta.resolve throws without arguments", () => {
    expect(() => import.meta.resolve()).toThrow(TypeError);
  });

  test("import.meta can be stored in a variable", () => {
    const meta = import.meta;
    expect(meta.url).toBe(import.meta.url);
  });

  test("import.meta.url in nested expression", () => {
    const parts = import.meta.url.split("/");
    expect(parts[parts.length - 1]).toBe("import-meta.js");
  });

  test("child module has different import.meta.url", () => {
    expect(childUrl).not.toBe(import.meta.url);
    expect(childUrl.includes("import-meta-child.js")).toBe(true);
  });

  test("child module import.meta is a different object", () => {
    expect(childMeta).not.toBe(import.meta);
  });

  test("child module import.meta.url starts with file://", () => {
    expect(childUrl.startsWith("file://")).toBe(true);
  });

  test("exported function preserves lexical import.meta binding", () => {
    // import.meta binds lexically: an exported function should report
    // the defining module's URL, not the calling module's URL
    const fnUrl = getUrl();
    expect(fnUrl.includes("import-meta-function.js")).toBe(true);
    expect(fnUrl).not.toBe(import.meta.url);
  });
});
