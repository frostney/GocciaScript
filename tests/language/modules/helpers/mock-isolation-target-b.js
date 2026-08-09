// Target B of the per-file mock isolation pair. vitest-module-mock-isolation-b.js
// mocks it; vitest-module-mock-isolation-a.js imports it without mocking and
// must still see this real value.
export const label = "REAL-ISOLATION-TARGET-B";
