import { describe, expect, test } from "bun:test";
import {
  gocciaToolInputZodSchema,
  MAX_GOCCIA_CODE_BYTES,
  MAX_GOCCIA_TOOL_REQUEST_BYTES,
  MAX_VERSION_BYTES,
  utf8ByteLength,
  validateGocciaToolInput,
} from "@/lib/goccia-tool-schema";

describe("goccia tool schema validation", () => {
  test("normalizes optional tool inputs", () => {
    const result = validateGocciaToolInput({
      code: "1 + 1;",
      mode: "bytecode",
      asi: false,
      compatVar: true,
    });

    expect(result).toEqual({
      ok: true,
      value: {
        code: "1 + 1;",
        mode: "bytecode",
        asi: false,
        compatVar: true,
        compatFunction: false,
      },
    });
  });

  test("accepts an explicit version tag", () => {
    const result = validateGocciaToolInput({
      code: "1;",
      version: "0.7.0",
    });
    expect(result).toEqual({
      ok: true,
      value: {
        code: "1;",
        mode: "interpreted",
        asi: true,
        compatVar: false,
        compatFunction: false,
        version: "0.7.0",
      },
    });
  });

  test("accepts the rolling nightly tag", () => {
    const result = validateGocciaToolInput({ code: "1;", version: "nightly" });
    expect(result.ok).toBe(true);
    if (result.ok) expect(result.value.version).toBe("nightly");
  });

  test("treats a missing version as undefined (server applies the default)", () => {
    const result = validateGocciaToolInput({ code: "1;" });
    expect(result.ok).toBe(true);
    if (result.ok) expect(result.value.version).toBeUndefined();
  });

  test("rejects an empty version string", () => {
    const result = validateGocciaToolInput({ code: "1;", version: "" });
    expect(result).toMatchObject({
      ok: false,
      error: { code: "INVALID_INPUT" },
    });
  });

  test("rejects a version exceeding the byte cap", () => {
    const result = validateGocciaToolInput({
      code: "1;",
      version: "x".repeat(MAX_VERSION_BYTES + 1),
    });
    expect(result).toMatchObject({
      ok: false,
      error: { code: "INVALID_INPUT" },
    });
  });

  test("rejects a non-string version", () => {
    const result = validateGocciaToolInput({ code: "1;", version: 7 });
    expect(result).toMatchObject({
      ok: false,
      error: { code: "INVALID_INPUT" },
    });
  });

  test("enforces the UTF-8 byte limit, not character count", () => {
    const code = "é".repeat(MAX_GOCCIA_CODE_BYTES / 2 + 1);

    expect(code.length).toBeLessThanOrEqual(MAX_GOCCIA_CODE_BYTES);
    expect(utf8ByteLength(code)).toBeGreaterThan(MAX_GOCCIA_CODE_BYTES);
    expect(validateGocciaToolInput({ code })).toEqual({
      ok: false,
      error: {
        code: "CODE_TOO_LARGE",
        message: `code exceeds ${MAX_GOCCIA_CODE_BYTES} bytes`,
      },
    });
  });

  test("derives the JSON wire cap from worst-case escaped code", () => {
    // The cap accounts for the full envelope shape — including a worst-case
    // length `version` field — plus worst-case JSON-escaped code. Mirror that
    // in the fixture so the bound stays tight.
    const worstVersion = "x".repeat(MAX_VERSION_BYTES);
    const envelope = (escapedCode: string) =>
      `{"code":"${escapedCode}","mode":"interpreted","asi":true,"compatVar":false,"compatFunction":false,"version":"${worstVersion}"}`;
    const escapedAtLimit = envelope("\\u0061".repeat(MAX_GOCCIA_CODE_BYTES));
    const escapedOverLimit = envelope(
      "\\u0061".repeat(MAX_GOCCIA_CODE_BYTES + 1),
    );

    expect(utf8ByteLength(escapedAtLimit)).toBeLessThanOrEqual(
      MAX_GOCCIA_TOOL_REQUEST_BYTES,
    );
    expect(utf8ByteLength(escapedOverLimit)).toBeGreaterThan(
      MAX_GOCCIA_TOOL_REQUEST_BYTES,
    );
    expect(validateGocciaToolInput(JSON.parse(escapedAtLimit))).toEqual({
      ok: true,
      value: {
        code: "a".repeat(MAX_GOCCIA_CODE_BYTES),
        mode: "interpreted",
        asi: true,
        compatVar: false,
        compatFunction: false,
        version: worstVersion,
      },
    });
  });

  test("rejects missing, empty, and invalid fields", () => {
    expect(validateGocciaToolInput(null)).toMatchObject({
      ok: false,
      error: { code: "INVALID_INPUT" },
    });
    expect(validateGocciaToolInput({})).toEqual({
      ok: false,
      error: {
        code: "MISSING_CODE",
        message: "Invalid input: expected string, received undefined",
      },
    });
    expect(validateGocciaToolInput({ code: "" })).toEqual({
      ok: false,
      error: {
        code: "MISSING_CODE",
        message: "code is required",
      },
    });
    expect(validateGocciaToolInput({ code: "1;", mode: "native" }).ok).toBe(
      false,
    );
  });

  test("exports the underlying Zod schema", () => {
    const result = gocciaToolInputZodSchema.safeParse({ code: "1;" });

    expect(result.success).toBe(true);
    if (result.success) {
      expect(result.data).toEqual({
        code: "1;",
        mode: "interpreted",
        asi: true,
        compatVar: false,
        compatFunction: false,
      });
    }
  });
});
