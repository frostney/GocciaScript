import { cycleValue } from "./live-bindings-cycle-a.js";

let observedCycleRead = "no error";

try {
  cycleValue;
} catch (error) {
  observedCycleRead = error.name;
}

export { observedCycleRead };
