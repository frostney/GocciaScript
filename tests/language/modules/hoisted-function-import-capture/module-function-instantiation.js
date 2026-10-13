/*---
description: A module's function declarations exist from linking, before its body runs, and read its environment live
features: [modules, compat-function]
---*/

import {
  asyncSeed,
  bump,
  earlyAsync,
  earlyCalls,
  earlyReadSeed,
  readCounter,
  readSeed,
  readSeedBinding,
  readSeedThroughNamespace,
  receiver,
  writeFrozen,
} from "./helpers/instantiation-cycle-a.js";
import { setSeed } from "./helpers/instantiation-seed.js";

describe("module function declarations are instantiated while linking", () => {
  test("a module in an import cycle calls them before their module body runs", () => {
    expect(earlyCalls.readSeed).toBe("seed-1");
    expect(earlyCalls.readSeedBinding).toBe("seed-1");
    expect(earlyCalls.readSeedThroughNamespace).toBe("seed-1");
    expect(earlyCalls.generator).toEqual(["seed-1", "internal:seed-1"]);
  });

  test("export default function is instantiated, named default", () => {
    expect(earlyCalls.defaultName).toBe("default");
    expect(earlyCalls.defaultResult).toBe("default:seed-1");
  });

  test("an async function declaration called before the body settles", async () => {
    expect(earlyAsync).toBeInstanceOf(Promise);
    expect(await earlyAsync).toBe("async:seed-1");
    expect(await asyncSeed()).toBe("async:seed-1");
  });

  test("the function a cycle saw is the one the module exports", () => {
    expect(earlyReadSeed).toBe(readSeed);
  });

  test("a lexical binding of the body is in its temporal dead zone until the body declares it", () => {
    expect(earlyCalls.bumpBeforeBody).toBe("ReferenceError");
    const start = readCounter();
    expect(bump()).toBe(start + 1);
    expect(readCounter()).toBe(start + 1);
  });

  test("imported bindings stay live", () => {
    setSeed("seed-2");
    try {
      expect(readSeed()).toBe("seed-2");
      expect(readSeedBinding()).toBe("seed-2");
      expect(readSeedThroughNamespace()).toBe("seed-2");
      expect(earlyReadSeed()).toBe("seed-2");
    } finally {
      setSeed("seed-1");
    }
  });

  test("function declarations are strict module code", () => {
    expect(earlyCalls.receiver).toBe(undefined);
    expect(receiver()).toBe(undefined);
    expect(() => writeFrozen()).toThrow(TypeError);
  });
});
