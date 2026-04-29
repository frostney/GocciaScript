export type ToolStep = {
  tool: string;
  call: string;
  role: string;
  result: string;
};

export type ToolDef = {
  type: "function";
  name: string;
  description: string;
  parameters: {
    type: "object";
    properties: Record<string, unknown>;
    required: string[];
  };
};

export type ToolFlow = {
  label: string;
  argName: "command" | "code";
  toolDef: ToolDef;
  steps: ToolStep[];
  risks: string[];
};

export type ToolFlowKey = "bash" | "goccia";

export const TOOL_CALL_TASK: string;
export const LLM_MODEL: string;
export const SYSTEM_PROMPT: string;
export const BASH_TOOL_PROMPT: string;
export const GOCCIA_TOOL_PROMPT: string;
export const USER_TASK: string;
export const BASH_TOOL: ToolDef;
export const RUN_CODE_TOOL: ToolDef;
export const TOOL_CALL_FLOWS: Record<ToolFlowKey, ToolFlow>;
export const LLM_CALL_TOKENS: Record<
  ToolFlowKey,
  { in: number[]; out: number[] }
>;

export function getInstructions(flowKey: ToolFlowKey): string;
export function buildFunctionCallOutput(
  flowKey: ToolFlowKey,
  index: number,
): unknown;
export function buildLlmRequest(flowKey: ToolFlowKey, index: number): unknown;
