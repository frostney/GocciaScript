"use client";

import { useEffect } from "react";
import { gocciaRunInputSchema } from "@/lib/goccia-tool-schema";

type GocciaToolInput = {
  code?: unknown;
  mode?: unknown;
  asi?: unknown;
  compatVar?: unknown;
  compatFunction?: unknown;
};

type ModelContextTool = {
  name: string;
  title: string;
  description: string;
  inputSchema: ReturnType<typeof gocciaRunInputSchema>;
  execute: (input: GocciaToolInput) => Promise<unknown>;
  annotations?: {
    readOnlyHint?: boolean;
    untrustedContentHint?: boolean;
  };
};

type ModelContext = {
  provideContext?: (context: {
    tools: ModelContextTool[];
  }) => void | Promise<void>;
  registerTool?: (
    tool: ModelContextTool,
    options?: { signal?: AbortSignal },
  ) => void | Promise<void>;
  clearContext?: () => void | Promise<void>;
};

type GocciaToolPayload = {
  code: string;
  mode: "interpreted" | "bytecode";
  asi: boolean;
  compatVar: boolean;
  compatFunction: boolean;
};

declare global {
  interface Navigator {
    modelContext?: ModelContext;
  }
}

function modelContextError(action: string, err: unknown) {
  console.warn(
    `[WebMCP] ${action} failed:`,
    err instanceof Error ? err.message : err,
  );
}

function normalizePayload(input: GocciaToolInput): GocciaToolPayload {
  const code = typeof input.code === "string" ? input.code : "";
  if (!code) throw new Error("code is required");

  return {
    code,
    mode: input.mode === "bytecode" ? "bytecode" : "interpreted",
    asi: typeof input.asi === "boolean" ? input.asi : true,
    compatVar: input.compatVar === true,
    compatFunction: input.compatFunction === true,
  };
}

async function callGocciaApi(
  endpoint: "/api/execute" | "/api/test",
  input: GocciaToolInput,
) {
  let payload: GocciaToolPayload;
  try {
    payload = normalizePayload(input);
  } catch (err) {
    return {
      endpoint,
      ok: false,
      status: null,
      error: {
        code: "INVALID_INPUT",
        message: err instanceof Error ? err.message : String(err),
      },
    };
  }

  let response: Response;
  try {
    response = await fetch(endpoint, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify(payload),
    });
  } catch (err) {
    return {
      endpoint,
      ok: false,
      status: null,
      payload,
      error: {
        code: "TRANSPORT_ERROR",
        message: err instanceof Error ? err.message : String(err),
      },
    };
  }

  let body: unknown = null;
  let parseError: string | null = null;
  try {
    body = await response.json();
  } catch (err) {
    parseError = err instanceof Error ? err.message : String(err);
  }

  return {
    endpoint,
    ok: response.ok,
    status: response.status,
    ...(parseError
      ? { error: { code: "INVALID_RESPONSE_JSON", message: parseError } }
      : {}),
    result: body,
  };
}

function buildTools(): ModelContextTool[] {
  const inputSchema = gocciaRunInputSchema();

  return [
    {
      name: "goccia.execute",
      title: "Run GocciaScript",
      description:
        "Execute a short GocciaScript program through the public website runtime API and return structured output.",
      inputSchema,
      execute: (input) => callGocciaApi("/api/execute", input),
      annotations: {
        readOnlyHint: true,
        untrustedContentHint: true,
      },
    },
    {
      name: "goccia.test",
      title: "Run GocciaScript Tests",
      description:
        "Execute a GocciaScript test file through the public website test runner API and return structured test results.",
      inputSchema,
      execute: (input) => callGocciaApi("/api/test", input),
      annotations: {
        readOnlyHint: true,
        untrustedContentHint: true,
      },
    },
  ];
}

export function WebMcpTools() {
  useEffect(() => {
    const modelContext = navigator.modelContext;
    if (!modelContext) return;

    const tools = buildTools();
    if (modelContext.provideContext) {
      void Promise.resolve(modelContext.provideContext({ tools })).catch(
        (err) => modelContextError("provideContext", err),
      );
      return () => {
        if (modelContext.clearContext) {
          void Promise.resolve(modelContext.clearContext()).catch((err) =>
            modelContextError("clearContext", err),
          );
        } else {
          void Promise.resolve(
            modelContext.provideContext?.({ tools: [] }),
          ).catch((err) => modelContextError("provideContext cleanup", err));
        }
      };
    }

    if (!modelContext.registerTool) return;
    const controller = new AbortController();
    for (const tool of tools) {
      if (controller.signal.aborted) break;
      void Promise.resolve(
        modelContext.registerTool(tool, { signal: controller.signal }),
      ).catch((err) => {
        if (!controller.signal.aborted) {
          modelContextError(`registerTool(${tool.name})`, err);
        }
      });
    }
    return () => controller.abort();
  }, []);

  return null;
}
