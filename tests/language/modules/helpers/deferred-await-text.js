// await in a comment must not make a deferred module eagerly evaluate.
const text = "await in a string is also inert";

globalThis.__gocciaDeferredAwaitTextEvaluated = true;

export const value = text.length;
