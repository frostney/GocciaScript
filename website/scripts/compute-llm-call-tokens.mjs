#!/usr/bin/env node
/**
 * One-shot helper: compute the GPT-5-family / o200k_base token count
 * of the shared Responses API request bodies used by the sandbox page.
 *
 *   node scripts/compute-llm-call-tokens.mjs
 *
 * `gpt-tokenizer` is a devDependency for this maintenance script only;
 * it is not imported by the website runtime.
 */

import { encode } from "gpt-tokenizer/esm/model/gpt-5";
import {
  buildFunctionCallOutput,
  buildLlmRequest,
  LLM_CALL_TOKENS,
  TOOL_CALL_FLOWS,
} from "../src/lib/tool-call-comparison.mjs";

const tokens = (obj) => encode(JSON.stringify(obj)).length;

const computed = Object.fromEntries(
  Object.entries(TOOL_CALL_FLOWS).map(([flowKey, flow]) => [
    flowKey,
    {
      in: flow.steps.map((_, i) => tokens(buildLlmRequest(flowKey, i))),
      out: flow.steps.map((_, i) =>
        tokens(buildFunctionCallOutput(flowKey, i)),
      ),
    },
  ]),
);

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

for (const [flowKey, flow] of Object.entries(TOOL_CALL_FLOWS)) {
  if (flowKey !== "bash") console.log();
  fmt(`${flow.label} flow`, computed[flowKey].in, computed[flowKey].out);
}

console.log(
  `\nFor src/lib/tool-call-comparison.mjs LLM_CALL_TOKENS:\n  bash:   { in: [${computed.bash.in.join(", ")}], out: [${computed.bash.out.join(", ")}] },\n  goccia: { in: [${computed.goccia.in.join(", ")}], out: [${computed.goccia.out.join(", ")}] },`,
);

if (JSON.stringify(computed) !== JSON.stringify(LLM_CALL_TOKENS)) {
  console.error("\nToken counts in LLM_CALL_TOKENS are stale.");
  process.exitCode = 1;
}
