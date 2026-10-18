// Imported by collection-during-cycle-evaluation.js. B imports this module
// back, so B evaluates first, while every export here is still in its
// temporal dead zone.
import { collectedDuringEvaluation, readA } from "./collection-cycle-b.js";

export const valueA = "from A";
export let counterA = 1;
export default "default from A";
export class ClassA {
  static label = "class A";
}

counterA = 2;

export { collectedDuringEvaluation, readA };
