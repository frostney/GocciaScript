// A module with top-level await that only a source phase import reaches (source-phase-experimental/deferred-source-phase.js): it must never
// be evaluated through that import.
await Promise.resolve();
globalThis.__gocciaDeferredSourceTargetRan = true;
export const value = 42;
