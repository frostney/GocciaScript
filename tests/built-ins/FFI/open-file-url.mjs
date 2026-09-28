// FFI.open with a file: URL; a module, for import.meta.url.

describe("FFI.open with a file URL", () => {
  test("opens a library named by a file URL", () => {
    const url = new URL(
      "../../../fixtures/ffi/libfixture" + FFI.suffix,
      import.meta.url,
    );
    const fromObject = FFI.open(url);
    expect(fromObject.closed).toBe(false);
    expect(fromObject.path).toBe(url.href);
    fromObject.close();
    const fromString = FFI.open(url.href);
    expect(fromString.closed).toBe(false);
    fromString.close();
  });

  test("judges a file URL as the path it names", () => {
    expect(() =>
      FFI.open(new URL("./nonexistent" + FFI.suffix, import.meta.url)),
    ).toThrow(PermissionDenied);
  });

  test("throws TypeError for a file URL that names no host path", () => {
    expect(() => FFI.open("file:///lib" + FFI.suffix + "?v=1")).toThrow(
      TypeError,
    );
    expect(() => FFI.open("file:///a%2Fb" + FFI.suffix)).toThrow(TypeError);
    expect(() => FFI.open("file:relative" + FFI.suffix)).toThrow(TypeError);
  });
});
