import {
  linkedFunctionMatches,
  observedCycleRead,
} from "./module-lexical-cycle-b.js";

const cycleValue = "cycle-ready";

export function readCycleValue() {
  return cycleValue;
}

export { linkedFunctionMatches, observedCycleRead };
