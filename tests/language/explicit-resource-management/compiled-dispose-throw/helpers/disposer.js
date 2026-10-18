// Module functions that perform the disposal, so a throw from the resource's
// dispose method is handled by their `using` / `await using` disposal. Two
// resources are tracked so the test can observe that disposal CONTINUES past a
// thrown exception (the throwing resource is declared last, so LIFO disposal
// runs it first). If disposal failed to catch the bytecode throw it would
// escape immediately and the tracker would never be disposed.
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
