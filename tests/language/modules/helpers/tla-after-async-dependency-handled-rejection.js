// A module with an asynchronous dependency and top-level await whose last
// statement leaves a handled, rejected promise.
import { dependencyReady } from "./tla-async-dependency.js";

export const afterDependencyValue = dependencyReady ? "loaded" : "not loaded";
await 0;
const rejected = Promise.reject(new Error("handled"));
rejected.catch(() => {});
rejected;
