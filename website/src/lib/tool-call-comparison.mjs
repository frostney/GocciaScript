export const TOOL_CALL_TASK =
  "Summarize the latest transaction batch and find outliers.";

export const LLM_MODEL = "gpt-5";
export const SYSTEM_PROMPT =
  "You are a financial-data analyst. Investigate the latest transaction batch and return a structured summary plus any outliers.";
export const BASH_TOOL_PROMPT = SYSTEM_PROMPT;
export const GOCCIA_TOOL_PROMPT = `${SYSTEM_PROMPT}

The host injects a global named transactions before running your GocciaScript code.
transactions is Array<{ id: number, amount: number }>.

GocciaScript is a strict ECMAScript subset. Do not use var, function declarations, loose equality (== / !=), eval, dynamic import, filesystem APIs, environment variables, or ambient host globals. Use const/let, arrow functions, strict equality, and the provided transactions global.`;
export const USER_TASK = TOOL_CALL_TASK;

export const BASH_TOOL = {
  type: "function",
  name: "bash",
  description: "Execute a shell command and return its stdout.",
  parameters: {
    type: "object",
    properties: {
      command: { type: "string", description: "The shell command to run." },
    },
    required: ["command"],
  },
};

export const RUN_CODE_TOOL = {
  type: "function",
  name: "run_code",
  description:
    "Run GocciaScript in a sandbox with host-provided globals; returns the script's value plus stdout.",
  parameters: {
    type: "object",
    properties: {
      code: {
        type: "string",
        description: "GocciaScript source. Use console.log for output.",
      },
    },
    required: ["code"],
  },
};

export const TOOL_CALL_FLOWS = {
  bash: {
    label: "Bash + jq",
    argName: "command",
    toolDef: BASH_TOOL,
    steps: [
      {
        tool: "bash",
        call: "ls /tmp/agent/transactions/",
        role: "discover",
        result: "transactions.current.json\n",
      },
      {
        tool: "bash",
        call: "cat /tmp/agent/transactions/transactions.current.json",
        role: "load",
        result:
          '[{"id":1,"amount":42.5},{"id":2,"amount":-12},{"id":3,"amount":98.34},{"id":4,"amount":-7.5},{"id":5,"amount":312},{"id":6,"amount":18.4},{"id":7,"amount":-298.4},{"id":8,"amount":54.2}]\n',
      },
      {
        tool: "bash",
        call: "jq '[.[].amount] | add' /tmp/agent/transactions/transactions.current.json",
        role: "sum",
        result: "207.54\n",
      },
      {
        tool: "bash",
        call: "jq '[.[].amount] | add / length' /tmp/agent/transactions/transactions.current.json",
        role: "average",
        result: "25.94\n",
      },
      {
        tool: "bash",
        call: "jq '[.[] | select((.amount | abs) > 280)]' /tmp/agent/transactions/transactions.current.json",
        role: "outliers",
        result: '[{"id":5,"amount":312},{"id":7,"amount":-298.4}]\n',
      },
    ],
    risks: [
      "values cross 5 process boundaries — possible to lose precision or quoting",
      "5 round-trips ≈ 5× the prompt overhead and 5× the chance of a misstep",
    ],
  },
  goccia: {
    label: "GocciaScript (single call)",
    argName: "code",
    toolDef: RUN_CODE_TOOL,
    steps: [
      {
        tool: "run_code",
        call: `const total = transactions.reduce((s, t) => s + t.amount, 0);
const avg = total / transactions.length;
const stdev = Math.sqrt(
  transactions.reduce((s, t) => s + (t.amount - avg) ** 2, 0) / transactions.length
);
const outliers = transactions.filter((t) => Math.abs(t.amount - avg) > 2 * stdev);
({ total, avg, outliers });`,
        role: "everything",
        result:
          '{"value":{"total":207.54,"avg":25.94,"outliers":[{"id":5,"amount":312},{"id":7,"amount":-298.4}]},"stdout":""}',
      },
    ],
    risks: [
      "all values stay in one sandbox — no serialization between steps",
      "host gets a single structured JSON result back",
    ],
  },
};

export const LLM_CALL_TOKENS = {
  bash: { in: [108, 166, 313, 382, 453], out: [39, 42, 50, 52, 58] },
  goccia: { in: [223], out: [138] },
};

export function getInstructions(flowKey) {
  return flowKey === "goccia" ? GOCCIA_TOOL_PROMPT : BASH_TOOL_PROMPT;
}

export function buildFunctionCallOutput(flowKey, index) {
  const flow = TOOL_CALL_FLOWS[flowKey];
  const step = flow.steps[index];
  return {
    type: "function_call",
    id: `fc_${index + 1}`,
    call_id: `call_${index + 1}`,
    name: step.tool,
    arguments: JSON.stringify({ [flow.argName]: step.call }),
  };
}

export function buildLlmRequest(flowKey, index) {
  const flow = TOOL_CALL_FLOWS[flowKey];
  const input = [{ role: "user", content: USER_TASK }];
  for (let j = 0; j < index; j++) {
    const prev = flow.steps[j];
    input.push(buildFunctionCallOutput(flowKey, j));
    input.push({
      type: "function_call_output",
      call_id: `call_${j + 1}`,
      output: prev.result,
    });
  }
  return {
    model: LLM_MODEL,
    instructions: getInstructions(flowKey),
    input,
    tools: [flow.toolDef],
  };
}
