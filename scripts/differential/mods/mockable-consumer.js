// Code under test: imports the mockable module the same way production code
// would, so the differential suite can prove the mock reaches a consumer and
// not just the test file's own binding.
import identify, { compute, label } from "./mockable.js";

export function readLabel() {
  return label;
}

export function callCompute(a, b) {
  return compute(a, b);
}

export function callDefault() {
  return identify();
}
