// Never loaded as itself: vitest-module-mock-keyword-exports.js mocks it with
// a factory whose keys are names that cannot be `const` bindings.
export const value = "REAL-KEYWORD-TARGET";
