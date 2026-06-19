import {
  first,
  third,
  rejectDone,
  resolveDone,
  resolveSecond,
} from "./deferred-async-cycle-promises.js";
import defer * as ns from "./deferred-async-cycle-target.js";

try {
  ns.foo;
} catch (error) {
  globalThis.__gocciaImportDeferErrorWhileEvaluating = error;
}

first
  .then(() => {
    try {
      ns.foo;
    } catch (error) {
      globalThis.__gocciaImportDeferErrorWhileEvaluatingAsync = error;
    }
    resolveSecond();
  })
  .then(() =>
    third.then(() => {
      globalThis.__gocciaImportDeferValueAfterEvaluated = ns.foo;
    }),
  )
  .then(resolveDone, rejectDone);
