import * as statically from "./helpers/stack-depth-static.js";
import * as chain from "./helpers/stack-depth-chain-outer.js";
import * as cycle from "./helpers/stack-depth-cycle-a.js";
import * as awaiting from "./helpers/stack-depth-tla.js";
import defer * as deferred from "./helpers/stack-depth-deferred.js";
import defer * as failing from "../../../fixtures/modules/deferred-evaluation-throws.js";

// The test runner's default --max-stack. A module's top level is not a call,
// so each helper's top level can make this many nested calls, less the calls
// live when it starts to evaluate.
const LIMIT = 2200;

describe("an imported module's top level allows --max-stack nested calls", () => {
  test("a statically imported module", () => {
    expect(statically.calls).toBe(LIMIT);
    expect(statically.error).toBeInstanceOf(RangeError);
  });

  test("each module of an import chain", () => {
    expect(chain.calls).toBe(LIMIT);
    expect(chain.innerCalls).toBe(LIMIT);
    expect(chain.error).toBeInstanceOf(RangeError);
  });

  test("each module of an import cycle", () => {
    expect(cycle.calls).toBe(LIMIT);
    expect(cycle.cycleBCalls).toBe(LIMIT);
    expect(cycle.error).toBeInstanceOf(RangeError);
  });

  test("a module with top-level await, before and after it awaits", () => {
    expect(awaiting.callsBeforeAwait).toBe(LIMIT);
    expect(awaiting.errorBeforeAwait).toBeInstanceOf(RangeError);
    expect(awaiting.callsAfterAwait).toBe(LIMIT);
    expect(awaiting.errorAfterAwait).toBeInstanceOf(RangeError);
  });

  test("a module loaded with import()", async () => {
    const dynamic = await import("./helpers/stack-depth-dynamic.js");
    expect(dynamic.calls).toBe(LIMIT);
    expect(dynamic.error).toBeInstanceOf(RangeError);
  });

  test("a deferred module evaluated four calls deep keeps the rest", () => {
    const readCalls = (k) => (k ? readCalls(k - 1) : deferred.calls);
    expect(readCalls(3)).toBe(LIMIT - 4);
    expect(deferred.error).toBeInstanceOf(RangeError);
  });

  test("a throw from a deferred module reaches the reader's catch", () => {
    const read = () => {
      try {
        return failing.value;
      } catch (e) {
        return e.message;
      }
    };
    expect(read()).toBe("deferred module failed");
  });
});
