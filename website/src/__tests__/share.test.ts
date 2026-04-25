import { describe, expect, test } from "bun:test";
import { decodeShare, encodeShare, type SharePayload } from "@/lib/share";

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
    // Encode a payload that's missing `code` entirely.
    const bytes = new TextEncoder().encode(
      JSON.stringify({ mode: "bytecode" }),
    );
    let bin = "";
    for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i]);
    const malformed = btoa(bin)
      .replace(/\+/g, "-")
      .replace(/\//g, "_")
      .replace(/=+$/, "");
    expect(decodeShare(malformed)).toBeNull();
  });

  test("drops non-string `mode` and `version`, non-boolean `asi`", () => {
    // Hand-encode a payload with the wrong types for the optional fields.
    const malformed = JSON.stringify({
      code: "ok;",
      mode: 42,
      asi: "yes",
      version: { tag: "v1.0.0" },
    });
    const bytes = new TextEncoder().encode(malformed);
    let bin = "";
    for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i]);
    const encoded = btoa(bin)
      .replace(/\+/g, "-")
      .replace(/\//g, "_")
      .replace(/=+$/, "");

    const decoded = decodeShare(encoded);
    expect(decoded).toEqual({
      code: "ok;",
      mode: undefined,
      asi: undefined,
      version: undefined,
    });
  });
});
