const events =
  globalThis.__gocciaImportDeferThenEvents ||
  (globalThis.__gocciaImportDeferThenEvents = []);

events.push("then-target");

export const exported = 42;
export const then = (callback) => {
  callback();
};
