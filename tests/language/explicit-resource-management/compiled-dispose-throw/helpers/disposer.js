// Interpreter-run (compat-function) functions that perform the disposal, so the
// interpreter's DisposeTrackedResources / DisposeTrackedResourcesAsync ladders
// handle a throw from the (VM-compiled) resource's dispose method. Two
// resources are tracked so the test can observe that disposal CONTINUES past a
// thrown boundary exception (the throwing resource is declared last, so LIFO
// disposal runs it first). If the ladder failed to catch the bytecode throw it
// would escape immediately and the tracker would never be disposed.
export async function disposeSync(tracker, thrower) {
  {
    using t = tracker;
    using x = thrower;
  }
}

export async function disposeAsync(tracker, thrower) {
  {
    await using t = tracker;
    await using x = thrower;
  }
}
