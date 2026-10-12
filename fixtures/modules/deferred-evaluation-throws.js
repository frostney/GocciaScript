// Evaluated through a deferred namespace read inside a try block
// (tests/language/modules/imported-module-stack-depth.js). It is a fixture, not
// a helper beside the test, because the test runner runs every file under
// tests/ and this one throws.
export const value = 1;

throw new Error("deferred module failed");
