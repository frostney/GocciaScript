// Re-exports the node_modules surface both goccia and bun must agree on.
// It sits next to `node_modules/` so the ancestor walk starts one directory
// above the packages, exactly as an application module would.
export { alphaLabel, chain } from "alpha";
export { widen, toolName } from "alpha/x/tool";
export { pinnedName } from "alpha/x/pinned";
export { scopedLabel } from "@fixture/scoped";
export { deepLabel } from "@fixture/scoped/deep";
