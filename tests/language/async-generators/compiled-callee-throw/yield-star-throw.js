/*---
description: interpreter-run async generator yield* over a compiled throwing sync next rejects with the thrown value itself
features: [async-generators, modules, compat-function]
---*/

// `delegate` is an imported module-level async generator, so under the bytecode
// executor it runs on the interpreter and its yield* uses the interpreter's
// async-from-sync iterator. The throwing sync iterable is created here, so its
// `next` is VM-compiled and its throw crosses that boundary as the engine's
// bytecode-throw exception. The rejection must carry the thrown value's
// identity, not a fresh Error synthesized from "Name: message".
import { delegate } from "./helpers/delegating-async-generator.js";

class Boom extends Error {
  constructor(message) {
    super(message);
    this.name = "Boom";
  }
}

describe("interpreter async generator yield* over a compiled throwing sync iterator", () => {
  test("rejects next() with the thrown error's identity", async () => {
    const boom = new Boom("boom");
    const throwingIterable = {
      [Symbol.iterator]() {
        return {
          next() {
            throw boom;
          },
        };
      },
    };

    let caught = null;
    try {
      await delegate(throwingIterable).next();
    } catch (error) {
      caught = error;
    }

    expect(caught).toBe(boom);
    expect(caught.name).toBe("Boom");
    expect(caught.message).toBe("boom");
  });

  test("rejects next() with a thrown non-error value's identity", async () => {
    const sentinel = { marker: "sentinel" };
    const throwingIterable = {
      [Symbol.iterator]() {
        return {
          next() {
            throw sentinel;
          },
        };
      },
    };

    let caught = "unset";
    try {
      await delegate(throwingIterable).next();
    } catch (error) {
      caught = error;
    }

    expect(caught).toBe(sentinel);
  });
});
