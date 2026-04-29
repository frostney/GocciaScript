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

declare global {
  interface Navigator {
    modelContext?: ModelContext;
  }
}

function normalizePayload(input: GocciaToolInput) {
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
  const payload = normalizePayload(input);
  const response = await fetch(endpoint, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(payload),
  });
  const body = await response.json().catch(() => null);

  return {
    endpoint,
    ok: response.ok,
    status: response.status,
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
      void modelContext.provideContext({ tools });
      return () => {
        if (modelContext.clearContext) {
          void modelContext.clearContext();
        } else {
          void modelContext.provideContext?.({ tools: [] });
        }
      };
    }

    if (!modelContext.registerTool) return;
    const controller = new AbortController();
    for (const tool of tools) {
      void modelContext.registerTool(tool, { signal: controller.signal });
    }
    return () => controller.abort();
  }, []);

  return null;
}
