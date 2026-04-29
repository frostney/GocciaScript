import { z } from "zod";

export const MAX_GOCCIA_CODE_BYTES = 8 * 1024;
const MAX_JSON_ESCAPE_BYTES_PER_CODE_BYTE = "\\uFFFF".length;
const REQUEST_ENVELOPE_BYTES = new TextEncoder().encode(
  JSON.stringify({
    code: "",
    mode: "interpreted",
    asi: true,
    compatVar: false,
    compatFunction: false,
  }),
).byteLength;

export const MAX_GOCCIA_TOOL_REQUEST_BYTES =
  REQUEST_ENVELOPE_BYTES +
  MAX_GOCCIA_CODE_BYTES * MAX_JSON_ESCAPE_BYTES_PER_CODE_BYTE;

export type GocciaToolPayload = {
  code: string;
  mode: "interpreted" | "bytecode";
  asi: boolean;
  compatVar: boolean;
  compatFunction: boolean;
};

export type GocciaToolInput = {
  code?: unknown;
  mode?: unknown;
  asi?: unknown;
  compatVar?: unknown;
  compatFunction?: unknown;
};

export type GocciaToolValidationResult =
  | { ok: true; value: GocciaToolPayload }
  | {
      ok: false;
      error: {
        code: "INVALID_INPUT" | "MISSING_CODE" | "CODE_TOO_LARGE";
        message: string;
      };
    };

export function utf8ByteLength(value: string) {
  return new TextEncoder().encode(value).byteLength;
}

export const gocciaToolInputZodSchema = z
  .object({
    code: z
      .string()
      .min(1, "code is required")
      .refine((code) => utf8ByteLength(code) <= MAX_GOCCIA_CODE_BYTES, {
        message: `code exceeds ${MAX_GOCCIA_CODE_BYTES} bytes`,
      }),
    mode: z.enum(["interpreted", "bytecode"]).default("interpreted"),
    asi: z.boolean().default(true),
    compatVar: z.boolean().default(false),
    compatFunction: z.boolean().default(false),
  })
  .strict();

export function validateGocciaToolInput(
  input: GocciaToolInput,
): GocciaToolValidationResult {
  const result = gocciaToolInputZodSchema.safeParse(input);
  if (result.success) return { ok: true, value: result.data };

  const issue = result.error.issues[0];
  const message = issue?.message ?? "invalid tool input";
  const inputRecord =
    input && typeof input === "object" && !Array.isArray(input)
      ? (input as Record<string, unknown>)
      : null;
  const errorCode =
    issue?.path[0] === "code" && message.includes("exceeds")
      ? "CODE_TOO_LARGE"
      : issue?.path[0] === "code" &&
          (inputRecord?.code === undefined || message === "code is required")
        ? "MISSING_CODE"
        : "INVALID_INPUT";

  return {
    ok: false,
    error: {
      code: errorCode,
      message,
    },
  };
}

export function gocciaRunInputSchema() {
  return {
    type: "object",
    additionalProperties: false,
    properties: {
      code: {
        type: "string",
        minLength: 1,
        description:
          "GocciaScript source code to execute. The server enforces the byte limit declared by maxBytes using UTF-8 byte length.",
        maxBytes: MAX_GOCCIA_CODE_BYTES,
      },
      mode: {
        type: "string",
        enum: ["interpreted", "bytecode"],
        description:
          "Execution backend. Use bytecode to match --mode=bytecode.",
        default: "interpreted",
      },
      asi: {
        type: "boolean",
        description: "Enable automatic semicolon insertion, matching --asi.",
        default: true,
      },
      compatVar: {
        type: "boolean",
        description: "Enable legacy var declarations, matching --compat-var.",
        default: false,
      },
      compatFunction: {
        type: "boolean",
        description: "Enable the function keyword, matching --compat-function.",
        default: false,
      },
    },
    required: ["code"],
  };
}
