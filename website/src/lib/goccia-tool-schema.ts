export const MAX_GOCCIA_CODE_BYTES = 8 * 1024;

export function gocciaRunInputSchema() {
  return {
    type: "object",
    additionalProperties: false,
    properties: {
      code: {
        type: "string",
        description: "GocciaScript source code to execute.",
        maxLength: MAX_GOCCIA_CODE_BYTES,
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
