// A module-level exported async generator. Its yield* wraps the sync iterable
// it is given in the VM's async-from-sync iterator, so a throw from that
// iterable's `next` reaches the wrapper as the engine's bytecode-throw
// exception. The iterable is created by the importing test.
export async function* delegate(iterable) {
  yield* iterable;
}
