// Target A of the per-file mock isolation pair. vitest-module-mock-isolation-a.js
// mocks it; vitest-module-mock-isolation-b.js imports it without mocking and
// must still see this real value.
export const label = "REAL-ISOLATION-TARGET-A";
