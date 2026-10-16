// Evaluates before collection-cycle-a.js, which it imports back. A's
// namespace object is live in this module's environment while A's exports
// are still in their temporal dead zone, and the collection marks through it.
import * as a from "./collection-cycle-a.js";

Goccia.gc();

export const collectedDuringEvaluation = true;

export const readA = () => ({
  value: a.valueA,
  counter: a.counterA,
  default: a.default,
  classLabel: a.ClassA.label,
});
