/*---
description: Class field initializers keep the defining module's host context on every route into the abstract Construct operation
features: [Reflect, dynamic-import, import-meta]
---*/

import {
  DefinitionContext,
  definingUrl,
} from "./helpers/class-definition-context.js";

// A field initializer is guest code, so constructing a class hands it the
// running module's `import()`, `import.meta` and file path. `new` gets those
// from the evaluation context it is already running in; Reflect.construct, a
// proxy without a construct trap, and a bound class reach the engine through
// the abstract Construct operation, where the caller is a native function with
// no context of its own. The context synthesized there must carry the same
// host services, sourced from the module the class was *defined* in — a
// missing module loader is not a degraded context but a crashing one, and a
// wrong file path silently resolves `import()` and `import.meta.url` against
// the wrong module.

const construct = {
  new: () => new DefinitionContext(),
  reflect: () => Reflect.construct(DefinitionContext, []),
  proxy: () => Reflect.construct(new Proxy(DefinitionContext, {}), []),
  bound: () => Reflect.construct(DefinitionContext.bind(null), []),
};

describe("import.meta in a class field initializer", () => {
  test("`new` resolves it against the defining module", () => {
    expect(construct.new().meta).toBe(definingUrl);
  });

  test("Reflect.construct resolves it against the defining module", () => {
    expect(construct.reflect().meta).toBe(definingUrl);
  });

  test("a proxy without a construct trap resolves it the same way", () => {
    expect(construct.proxy().meta).toBe(definingUrl);
  });

  test("a bound class resolves it the same way", () => {
    expect(construct.bound().meta).toBe(definingUrl);
  });

  test("it is the defining module's URL, not the constructing module's", () => {
    expect(definingUrl).not.toBe(import.meta.url);
    expect(definingUrl.endsWith("class-definition-context.js")).toBe(true);
    expect(construct.reflect().meta).not.toBe(import.meta.url);
  });
});

describe("dynamic import() in a class field initializer", () => {
  test("`new` resolves the specifier against the defining module", async () => {
    const mod = await construct.new().dep;
    expect(mod.add(2, 3)).toBe(5);
  });

  test("Reflect.construct resolves the specifier the same way", async () => {
    const mod = await construct.reflect().dep;
    expect(mod.add(2, 3)).toBe(5);
  });

  test("a proxy without a construct trap resolves it the same way", async () => {
    const mod = await construct.proxy().dep;
    expect(mod.multiply(3, 4)).toBe(12);
  });

  test("a bound class resolves it the same way", async () => {
    const mod = await construct.bound().dep;
    expect(mod.PI).toBe(3.14159);
  });

  test("every route reaches the same module instance", async () => {
    const mods = await Promise.all([
      construct.new().dep,
      construct.reflect().dep,
      construct.proxy().dep,
      construct.bound().dep,
    ]);
    expect(mods[1]).toBe(mods[0]);
    expect(mods[2]).toBe(mods[0]);
    expect(mods[3]).toBe(mods[0]);
  });
});
