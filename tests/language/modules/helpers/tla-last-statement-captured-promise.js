// A module with an asynchronous dependency whose last statement leaves
// whatever promise a patched Promise.prototype.then last saw, which can be
// the module's own evaluation promise. Its evaluation still succeeds.
import { dependencyReady } from "./tla-async-dependency.js";

export const capturedValue = dependencyReady ? "loaded" : "not loaded";
await new Promise((resolve) => {
  let count = 0;
  const tick = () => {
    count = count + 1;
    if (count === 10) {
      resolve();
    } else {
      queueMicrotask(tick);
    }
  };
  queueMicrotask(tick);
});
globalThis.capturedByThen;
