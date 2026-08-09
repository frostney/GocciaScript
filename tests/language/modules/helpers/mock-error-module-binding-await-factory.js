// Never actually loaded as itself: vitest-module-mock-unsupported-factories.js
// mocks it with a factory whose object literal uses "await" as a key, which
// no module may bind even though it is not a keyword.
export const value = 0;
