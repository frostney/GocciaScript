// A module-level exported async generator. With compat-function enabled the
// module loader creates it as an interpreter closure even under the bytecode
// executor, so its yield* delegation runs through the interpreter's
// async-from-sync iterator (Goccia.Generator.Continuation). The sync iterable
// it delegates to is created by the VM-compiled caller, so that iterable's
// `next` throw crosses the boundary as the engine's bytecode-throw exception.
export async function* delegate(iterable) {
  yield* iterable;
}
