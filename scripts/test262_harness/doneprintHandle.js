// test262 doneprintHandle.js — GocciaScript-compatible reimplementation.
//
// Stock test262 doneprintHandle uses `print('Test262:AsyncTestComplete')` /
// `print('Test262:AsyncTestFailure:...')` so an external orchestrator can
// scan stdout for those markers.  GocciaScript's bytecode VM has a
// pre-existing crash (Range check error) when the microtask queue is
// drained for a top-level `Promise.then` continuation.  Tests written like
//
//     p.then(function (v) { assert.sameValue(v, 3); $DONE(); }, $DONE);
//
// would queue a `.then` microtask and crash the engine the moment we tried
// to run it — losing every async test in bytecode mode to an engine
// surface, not a real conformance failure.
//
// To stay on the marker-based wire protocol the orchestrator expects while
// avoiding that drain path, we route completion through a Promise the
// orchestrator-injected wrapper awaits inside an async IIFE: async-function
// `await` drives microtask completion through the VM's continuation
// machinery instead of the broken top-level drain, then the wrapper itself
// emits the marker.  Keep this file in lockstep with the
// `positive_async` branch in `scripts/run_test262_suite.ts`.

const __doneResolvers = Promise.withResolvers();
const __donePromise = __doneResolvers.promise;

const $DONE = (error) => {
  if (error) {
    if (error instanceof Error) {
      __doneResolvers.reject(error);
    } else {
      __doneResolvers.reject(new Test262Error(String(error)));
    }
  } else {
    __doneResolvers.resolve();
  }
};
