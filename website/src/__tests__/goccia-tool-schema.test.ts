import { describe, expect, test } from "bun:test";
import {
  gocciaToolInputZodSchema,
  MAX_GOCCIA_CODE_BYTES,
  MAX_GOCCIA_TOOL_REQUEST_BYTES,
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
    const escapedAtLimit = `{"code":"${"\\u0061".repeat(MAX_GOCCIA_CODE_BYTES)}","mode":"interpreted","asi":true,"compatVar":false,"compatFunction":false}`;
    const escapedOverLimit = `{"code":"${"\\u0061".repeat(MAX_GOCCIA_CODE_BYTES + 1)}","mode":"interpreted","asi":true,"compatVar":false,"compatFunction":false}`;

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
      },
    });
  });

  test("rejects invalid required fields and enum values", () => {
    expect(validateGocciaToolInput({}).ok).toBe(false);
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
