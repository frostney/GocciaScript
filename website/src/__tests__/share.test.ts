import { describe, expect, test } from "bun:test";
import { decodeShare, encodeShare, type SharePayload } from "@/lib/share";

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
      asi: false,
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

  test("drops non-string `mode` and `version`, non-boolean `asi`", () => {
    const decoded = decodeShare(
      encodeJsonBase64Url({
        code: "ok;",
        mode: 42,
        asi: "yes",
        version: { tag: "v1.0.0" },
      }),
    );
    expect(decoded).toEqual({
      code: "ok;",
      mode: undefined,
      asi: undefined,
      version: undefined,
    });
  });
});
