export const MAX_GOCCIA_CODE_BYTES = 8 * 1024;

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
        code: "INVALID_INPUT" | "CODE_TOO_LARGE";
        message: string;
      };
    };

export function utf8ByteLength(value: string) {
  return new TextEncoder().encode(value).byteLength;
}

export function validateGocciaToolInput(
  input: GocciaToolInput,
): GocciaToolValidationResult {
  const code = typeof input.code === "string" ? input.code : "";
  if (!code) {
    return {
      ok: false,
      error: {
        code: "INVALID_INPUT",
        message: "code is required",
      },
    };
  }

  const codeBytes = utf8ByteLength(code);
  if (codeBytes > MAX_GOCCIA_CODE_BYTES) {
    return {
      ok: false,
      error: {
        code: "CODE_TOO_LARGE",
        message: `code exceeds ${MAX_GOCCIA_CODE_BYTES} bytes`,
      },
    };
  }

  if (
    input.mode !== undefined &&
    input.mode !== "interpreted" &&
    input.mode !== "bytecode"
  ) {
    return {
      ok: false,
      error: {
        code: "INVALID_INPUT",
        message: "mode must be interpreted or bytecode",
      },
    };
  }

  return {
    ok: true,
    value: {
      code,
      mode: input.mode === "bytecode" ? "bytecode" : "interpreted",
      asi: typeof input.asi === "boolean" ? input.asi : true,
      compatVar: input.compatVar === true,
      compatFunction: input.compatFunction === true,
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
