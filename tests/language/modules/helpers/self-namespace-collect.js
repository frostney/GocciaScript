import * as self from "./self-namespace-collect.js";

// Collects while this module's own exports are still uninitialized, with its
// namespace object alive.
if (typeof Goccia !== "undefined") Goccia.gc();

export const value = "initialized";
export const readThroughNamespace = () => self.value;
