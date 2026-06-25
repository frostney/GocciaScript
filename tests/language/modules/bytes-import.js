import bytesDefault from "./helpers/asset.bin" with { type: "bytes" };
import { default as aliasedDefault } from "./helpers/asset.bin" with { type: "bytes" };
import * as bytesNamespace from "./helpers/asset.bin" with { type: "bytes" };
import rawJson from "./helpers/config.json" with { type: "bytes" };

const EXPECTED = [0, 1, 255, 254, 13, 10, 65];

describe("bytes module import", () => {
  test("default export is a Uint8Array of the file length", () => {
    expect(bytesDefault instanceof Uint8Array).toBe(true);
    expect(bytesDefault.length).toBe(7);
  });

  test("preserves exact bytes including NUL, non-UTF-8, and CRLF", () => {
    expect(Array.from(bytesDefault)).toEqual(EXPECTED);
  });

  test("is backed by an immutable ArrayBuffer", () => {
    expect(bytesDefault.buffer instanceof ArrayBuffer).toBe(true);
    expect(bytesDefault.buffer.immutable).toBe(true);
  });

  test("ignores indexed writes to the immutable bytes view", () => {
    bytesDefault[0] = 99;
    expect(bytesDefault[0]).toBe(0);
  });

  test("declares only a default export, so named bindings are absent", () => {
    expect(bytesNamespace.default).toBe(bytesDefault);
    expect(Object.keys(bytesNamespace)).toEqual(["default"]);
    expect(bytesNamespace.data).toBe(undefined);
  });

  test("shares the module cache across matching static imports", () => {
    expect(aliasedDefault).toBe(bytesDefault);
    expect(bytesNamespace.default).toBe(bytesDefault);
  });

  test("ignores file extension and MIME, loading config.json as raw bytes", () => {
    expect(rawJson instanceof Uint8Array).toBe(true);
    expect(rawJson[0]).toBe(123);
  });

  test("dynamic import resolves to the same cached bytes module", async () => {
    const dyn = await import("./helpers/asset.bin", { with: { type: "bytes" } });
    expect(dyn.default instanceof Uint8Array).toBe(true);
    expect(dyn.default).toBe(bytesDefault);
  });

  test("two dynamic bytes imports share one module", async () => {
    const a = await import("./helpers/asset.bin", { with: { type: "bytes" } });
    const b = await import("./helpers/asset.bin", { with: { type: "bytes" } });
    expect(a.default).toBe(b.default);
  });

  test("import.defer resolves a default-only bytes namespace", async () => {
    const deferred = await import.defer("./helpers/asset.bin", {
      with: { type: "bytes" },
    });
    expect(deferred.default instanceof Uint8Array).toBe(true);
    expect(deferred.default.length).toBe(7);
  });

  test("import.source rejects bytes modules", async () => {
    await expect(
      import.source("./helpers/asset.bin", { with: { type: "bytes" } }),
    ).rejects.toThrow(SyntaxError);
  });
});
