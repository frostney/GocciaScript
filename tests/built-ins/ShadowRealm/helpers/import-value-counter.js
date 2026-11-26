// Fixture module for import-value-own-module-record.js: module state a realm
// can observe. Runs as a standalone (import-only) file in the test runner
// without registering tests.
let count = 0;
export const bump = () => ++count;
export const marker = () => globalThis.__importValueRealmMarker;
