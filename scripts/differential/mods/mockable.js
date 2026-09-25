// Differential fixture: the module h-modulemock.test.js replaces with a
// vi.mock factory. Every export here is the REAL value, so a differential suite that
// reads "REAL_*" is seeing through the mock.
export const label = "REAL_LABEL";

export function compute(a, b) {
  return a + b;
}

export default function identify() {
  return "REAL_DEFAULT";
}
