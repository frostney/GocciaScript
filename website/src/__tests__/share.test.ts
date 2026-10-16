import { describe, expect, test } from "bun:test";
import {
  decodeShare,
  effectiveMode,
  encodeShare,
  isModeSelectable,
  type PlaygroundModeSupport,
  type SharePayload,
} from "@/lib/share";

/** Encode an arbitrary value as base64url-wrapped JSON, matching what
 *  `encodeShare` produces. Used by tests that need to inject malformed
 *  or wrong-typed payloads — `encodeShare` itself is type-safe and
 *  can't produce them. */
function encodeJsonBase64Url(value: unknown): string {
  const json = typeof value === "string" ? value : JSON.stringify(value);
  const bytes = new TextEncoder().encode(json);
  let bin = "";
  for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i]);
  return btoa(bin).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

describe("share encoding", () => {
  test("round-trips a complete payload", () => {
    const payload: SharePayload = {
      code: "console.log('Hello, GocciaScript!');\n",
      mode: "bytecode",
      runner: "test",
      asi: false,
      compatVar: true,
      compatFunction: true,
      version: "v0.6.1",
    };
    const encoded = encodeShare(payload);
    const decoded = decodeShare(encoded);
    expect(decoded).toEqual(payload);
  });

  test("round-trips code containing non-BMP / multi-byte characters", () => {
    // Includes a 4-byte emoji and a 2-byte glyph — exercises the
    // `TextEncoder` → bytes → base64url → `TextDecoder` path.
    const payload: SharePayload = {
      code: "const greet = '☕ Goccia 🚀';",
    };
    const decoded = decodeShare(encodeShare(payload));
    expect(decoded?.code).toBe(payload.code);
  });

  test("encoded form is URL-safe (no `+`, `/`, `=`)", () => {
    const longCode = "x".repeat(300);
    const encoded = encodeShare({ code: longCode });
    expect(encoded).not.toMatch(/[+/=]/);
  });

  test("rejects malformed base64", () => {
    expect(decodeShare("not!valid!base64!")).toBeNull();
  });

  test("rejects payload without a string `code` field", () => {
    expect(decodeShare(encodeJsonBase64Url({ mode: "bytecode" }))).toBeNull();
  });

  test("drops invalid optional fields", () => {
    const decoded = decodeShare(
      encodeJsonBase64Url({
        code: "ok;",
        mode: 42,
        runner: "bench",
        asi: "yes",
        compatVar: "yes",
        compatFunction: 1,
        version: { tag: "v1.0.0" },
      }),
    );
    expect(decoded).toEqual({
      code: "ok;",
      mode: undefined,
      runner: undefined,
      asi: undefined,
      compatVar: undefined,
      compatFunction: undefined,
      version: undefined,
    });
  });
});

describe("playground execution mode", () => {
  const support: PlaygroundModeSupport = {
    "v0.14.0": { loader: true, testRunner: true },
    "v0.6.1": { loader: false, testRunner: false },
    "v0.9.0": { loader: true, testRunner: false },
  };

  test("a version is mode-selectable per runner, and an unknown one is not", () => {
    expect(isModeSelectable(support, "v0.14.0", "execute")).toBe(true);
    expect(isModeSelectable(support, "v0.6.1", "execute")).toBe(false);
    expect(isModeSelectable(support, "v0.9.0", "execute")).toBe(true);
    expect(isModeSelectable(support, "v0.9.0", "test")).toBe(false);
    // An unknown tag is a locally built engine, which has no --mode since
    // 0.15.0, so the playground shows no toggle for it.
    expect(isModeSelectable(support, "local-build", "test")).toBe(false);
    expect(isModeSelectable(undefined, "v0.6.1", "execute")).toBe(false);
  });

  test("a selectable version carries the selected mode", () => {
    expect(effectiveMode("interpreted", true)).toBe("interpreted");
    expect(effectiveMode("bytecode", true)).toBe("bytecode");
  });

  test("a hidden backend carries no mode, whatever it holds", () => {
    expect(effectiveMode("interpreted", false)).toBeUndefined();
    expect(effectiveMode("bytecode", false)).toBeUndefined();
  });

  test("an unknown or missing mode carries none", () => {
    expect(effectiveMode("jit", true)).toBeUndefined();
    expect(effectiveMode(undefined, true)).toBeUndefined();
  });

  test("a share link for a version without --mode records no mode", () => {
    const encoded = encodeShare({
      code: "1;",
      mode: effectiveMode("interpreted", false),
      version: "v0.6.1",
    });
    expect(decodeShare(encoded)?.mode).toBeUndefined();
  });

  test("hydrating a link restores its mode only where the version takes --mode", () => {
    const decoded = decodeShare(
      encodeShare({ code: "1;", mode: "interpreted", version: "v0.6.1" }),
    );
    expect(
      effectiveMode(
        decoded?.mode,
        isModeSelectable(support, "v0.6.1", "execute"),
      ),
    ).toBeUndefined();
    expect(
      effectiveMode(
        decoded?.mode,
        isModeSelectable(support, "v0.14.0", "execute"),
      ),
    ).toBe("interpreted");
  });
});
