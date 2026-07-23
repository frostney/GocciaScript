import {
  linkedCycleReady,
  observedLinkedCycle,
} from "./live-bindings-link-child.js";
import * as linkedCycleNamespace from "./live-bindings-link-child.js";

export function readLinkedCycleReady() {
  return linkedCycleReady + ":" + linkedCycleNamespace.linkedCycleReady;
}

export { observedLinkedCycle };
