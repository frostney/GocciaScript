// Placeholder target (automock) for the vi.mock shapes the shim cannot generate a module
// for. It must exist on disk: a mock of a module that cannot be resolved is
// dropped at hoist time, so the import would fail with module-not-found
// instead of with the shim's own diagnostic.
export const label = "REAL-ERROR-TARGET";
