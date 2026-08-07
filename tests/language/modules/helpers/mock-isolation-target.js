// Shared target of the per-file mock isolation pair
// (vitest-module-mock-isolation-a-mocked.js mocks it,
// vitest-module-mock-isolation-b-real.js must still see this real value).
export const label = "REAL-ISOLATION-TARGET";
