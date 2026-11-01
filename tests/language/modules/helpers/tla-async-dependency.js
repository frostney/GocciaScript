// A dependency with top-level await, so a module importing it evaluates
// asynchronously.
await 0;
export const dependencyReady = true;
