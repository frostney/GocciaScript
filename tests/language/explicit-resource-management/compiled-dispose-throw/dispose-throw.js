/*---
description: interpreter-run disposal catches a compiled dispose throw, keeps disposing, and surfaces the thrown value
features: [explicit-resource-management, modules, compat-function]
---*/

// disposeSync/disposeAsync are imported module-level async functions, so under
// the bytecode executor they run on the interpreter and their disposal uses the
// interpreter's DisposeTrackedResources / DisposeTrackedResourcesAsync ladders.
// The resources are created here, so their dispose methods are VM-compiled and a
// throw crosses that boundary as the engine's bytecode-throw exception. The
// ladder must CATCH that exception (so a later resource is still disposed) and
// surface the thrown value's identity — not let it escape uncaught.
import { disposeSync, disposeAsync } from "./helpers/disposer.js";

class Boom extends Error {
  constructor(message) {
    super(message);
    this.name = "Boom";
  }
}

describe("interpreter disposal of a compiled throwing resource", () => {
  test("using [Symbol.dispose] throw is caught; later resource still disposed", async () => {
    const boom = new Boom("boom");
    let trackerDisposed = false;
    const tracker = {
      [Symbol.dispose]() {
        trackerDisposed = true;
      },
    };
    const thrower = {
      [Symbol.dispose]() {
        throw boom;
      },
    };

    let caught = null;
    try {
      await disposeSync(tracker, thrower);
    } catch (error) {
      caught = error;
    }

    // If the bytecode throw escaped uncaught, disposal would stop before the
    // tracker (declared first, disposed last) ran.
    expect(trackerDisposed).toBe(true);
    expect(caught).toBe(boom);
    expect(caught.name).toBe("Boom");
    expect(caught.message).toBe("boom");
  });

  test("await using synchronous dispose throw is caught; later resource still disposed", async () => {
    const boom = new Boom("boom");
    let trackerDisposed = false;
    const tracker = {
      async [Symbol.asyncDispose]() {
        trackerDisposed = true;
      },
    };
    // A synchronous [Symbol.dispose] under await using throws before the await
    // point, so its bytecode throw reaches the async disposal ladder as the
    // engine's bytecode-throw exception (an async dispose would instead reject a
    // promise, surfacing as a plain interpreter throw).
    const thrower = {
      [Symbol.dispose]() {
        throw boom;
      },
    };

    let caught = null;
    try {
      await disposeAsync(tracker, thrower);
    } catch (error) {
      caught = error;
    }

    expect(trackerDisposed).toBe(true);
    expect(caught).toBe(boom);
    expect(caught.name).toBe("Boom");
    expect(caught.message).toBe("boom");
  });
});
