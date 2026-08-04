import { readCycleValue } from "./module-lexical-cycle-a.js";

const linkedReadCycleValue = readCycleValue;
let observedCycleRead = "no error";

try {
  readCycleValue();
} catch (error) {
  observedCycleRead = error.name;
}

function linkedFunctionMatches() {
  return linkedReadCycleValue === readCycleValue;
}

export { linkedFunctionMatches, observedCycleRead };
