import { z } from "zod";

export const MAX_GOCCIA_CODE_BYTES = 8 * 1024;
/** Upper bound on `version` so the wire-size envelope stays predictable.
 *  Real tags are short (`"v0.7.0"`, `"nightly"`); 64 bytes is comfortably
 *  above any plausible release tag while still bounding the JSON envelope. */
export const MAX_VERSION_BYTES = 64;
const MAX_JSON_ESCAPE_BYTES_PER_CODE_BYTE = "\\uFFFF".length;
const REQUEST_ENVELOPE_BYTES = new TextEncoder().encode(
  JSON.stringify({
    code: "",
    mode: "interpreted",
    asi: true,
    compatVar: false,
    compatFunction: false,
    // Worst-case `version` length is included in the envelope so the
    // request-byte cap accounts for the full payload shape.
    version: "x".repeat(MAX_VERSION_BYTES),
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
  /** Engine release tag the request should run against (e.g. `"v0.7.0"`,
   *  `"nightly"`). When omitted, the API defaults to `manifest.defaultVersion`. */
  version?: string;
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
    // Optional, length-bounded — the API resolves the value against
    // `vendor/manifest.json` at request time. Validating against an enum
    // here would couple the schema to build artifacts.
    version: z
      .string()
      .min(1, "version cannot be empty")
      .max(MAX_VERSION_BYTES, `version exceeds ${MAX_VERSION_BYTES} bytes`)
      .optional(),
  })
  .strict();

export function validateGocciaToolInput(
  input: unknown,
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
      version: {
        type: "string",
        minLength: 1,
        maxLength: MAX_VERSION_BYTES,
        description:
          'Optional engine release tag (e.g. "0.7.0", "nightly"). The server resolves the value against the vendored binary manifest; unknown tags fail with code UNKNOWN_VERSION. Omit to use the default version.',
      },
    },
    required: ["code"],
  };
}
