// A module with an asynchronous dependency and top-level await whose last
// statement leaves a thenable.
import { dependencyReady } from "./tla-async-dependency.js";

export let afterDependencyThenCalls = 0;
export const afterDependencyThenableValue = dependencyReady
  ? "loaded"
  : "not loaded";
await 0;
({
  then() {
    afterDependencyThenCalls = afterDependencyThenCalls + 1;
  },
});
