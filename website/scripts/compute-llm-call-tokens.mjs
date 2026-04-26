#!/usr/bin/env node
/**
 * One-shot helper: compute the tiktoken count of the FULL LLM request
 * body at each tool-call turn for both flows shown in
 * `src/components/sandbox.tsx`. Re-run if the system prompt, tool
 * definitions, step calls, or tool-result texts change in that file;
 * paste the printed arrays back into `LLM_CALL_TOKENS`.
 *
 *   bun add -d gpt-tokenizer
 *   node scripts/compute-llm-call-tokens.mjs
 *   bun remove gpt-tokenizer
 *
 * The page can't ship `gpt-tokenizer` (~53 MB unpacked), so this stays
 * as a maintenance-only helper outside the dependency graph.
 */

import { encode } from "gpt-tokenizer/model/gpt-4";

const MODEL = "gpt-4";
const SYSTEM_PROMPT =
  "You are a financial-data analyst. The host has put a transactions array in your reach. Investigate it and return a structured summary plus any outliers.";
const USER_TASK = "Summarize the latest transaction batch and find outliers.";

const BASH_TOOL = {
  type: "function",
  function: {
    name: "bash",
    description: "Execute a shell command and return its stdout.",
    parameters: {
      type: "object",
      properties: {
        command: { type: "string", description: "The shell command to run." },
      },
      required: ["command"],
    },
  },
};

const RUN_CODE_TOOL = {
  type: "function",
  function: {
    name: "run_code",
    description:
      "Run GocciaScript in a sandbox with the provided globals; returns the script's value plus stdout. The sandbox has no fetch, fs, env, or eval.",
    parameters: {
      type: "object",
      properties: {
        code: {
          type: "string",
          description: "GocciaScript source. Last expression is the result.",
        },
        globals: {
          type: "object",
          description: "Variables injected into the sandbox.",
          additionalProperties: true,
        },
      },
      required: ["code"],
    },
  },
};

// Mirror the steps in `TOOL_CALL_FLOWS.bash` in sandbox.tsx.
const BASH_STEPS = [
  {
    call: "ls /tmp/agent/transactions/",
    result: "transactions.current.json\n",
  },
  {
    call: "cat /tmp/agent/transactions/transactions.current.json",
    result:
      '[{"id":1,"amount":42.5},{"id":2,"amount":-12},{"id":3,"amount":98.34},{"id":4,"amount":-7.5},{"id":5,"amount":312},{"id":6,"amount":18.4},{"id":7,"amount":-298.4},{"id":8,"amount":54.2}]\n',
  },
  {
    call: "jq '[.[].amount] | add' transactions.current.json",
    result: "207.54\n",
  },
  {
    call: "jq '[.[].amount] | add / length' transactions.current.json",
    result: "25.94\n",
  },
  {
    call: "jq '[.[] | select(.amount > 280)]' transactions.current.json",
    result: '[{"id":5,"amount":312},{"id":7,"amount":-298.4}]\n',
  },
];

function bashRequestAt(stepIndex) {
  const messages = [
    { role: "system", content: SYSTEM_PROMPT },
    { role: "user", content: USER_TASK },
  ];
  for (let j = 0; j < stepIndex; j++) {
    messages.push({
      role: "assistant",
      content: null,
      tool_calls: [
        {
          id: `call_${j + 1}`,
          type: "function",
          function: {
            name: "bash",
            arguments: JSON.stringify({ command: BASH_STEPS[j].call }),
          },
        },
      ],
    });
    messages.push({
      role: "tool",
      tool_call_id: `call_${j + 1}`,
      content: BASH_STEPS[j].result,
    });
  }
  return { model: MODEL, messages, tools: [BASH_TOOL] };
}

function gocciaRequest() {
  return {
    model: MODEL,
    messages: [
      { role: "system", content: SYSTEM_PROMPT },
      { role: "user", content: USER_TASK },
    ],
    tools: [RUN_CODE_TOOL],
  };
}

const tokens = (obj) => encode(JSON.stringify(obj)).length;

/** Output tokens per step = the assistant message the model emits for
 *  that turn. Same shape we'd append to `messages` for the next call. */
function bashOutputAt(stepIndex) {
  return {
    role: "assistant",
    content: null,
    tool_calls: [
      {
        id: `call_${stepIndex + 1}`,
        type: "function",
        function: {
          name: "bash",
          arguments: JSON.stringify({ command: BASH_STEPS[stepIndex].call }),
        },
      },
    ],
  };
}

const GOCCIA_CALL_BODY = `const total = transactions.reduce((s, t) => s + t.amount, 0);
const avg = total / transactions.length;
const stdev = Math.sqrt(
  transactions.reduce((s, t) => s + (t.amount - avg) ** 2, 0) / transactions.length
);
const outliers = transactions.filter((t) => Math.abs(t.amount - avg) > 2 * stdev);
({ total, avg, outliers });`;

const gocciaOutput = {
  role: "assistant",
  content: null,
  tool_calls: [
    {
      id: "call_1",
      type: "function",
      function: {
        name: "run_code",
        arguments: JSON.stringify({ code: GOCCIA_CALL_BODY }),
      },
    },
  ],
};

const bashIn = BASH_STEPS.map((_, i) => tokens(bashRequestAt(i)));
const bashOut = BASH_STEPS.map((_, i) => tokens(bashOutputAt(i)));
const gocciaIn = [tokens(gocciaRequest())];
const gocciaOut = [tokens(gocciaOutput)];

const fmt = (label, ins, outs) => {
  console.log(`== ${label} ==`);
  ins.forEach((tIn, i) => {
    console.log(
      `  step ${i + 1}:  in=${String(tIn).padStart(4)} tok   out=${String(outs[i]).padStart(3)} tok`,
    );
  });
  const totIn = ins.reduce((s, t) => s + t, 0);
  const totOut = outs.reduce((s, t) => s + t, 0);
  console.log(
    `  TOTAL:   in=${String(totIn).padStart(4)} tok   out=${String(totOut).padStart(3)} tok   sum=${totIn + totOut}`,
  );
};

fmt("Bash + jq flow (5 round-trips)", bashIn, bashOut);
console.log();
fmt("GocciaScript flow (1 call)", gocciaIn, gocciaOut);

console.log(
  `\nFor sandbox.tsx LLM_CALL_TOKENS:\n  bash:   { in: [${bashIn.join(", ")}], out: [${bashOut.join(", ")}] },\n  goccia: { in: [${gocciaIn.join(", ")}], out: [${gocciaOut.join(", ")}] },`,
);
