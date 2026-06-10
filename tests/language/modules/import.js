import { localFunction, localValue } from "./local-module.js";
import defer * as deferredMath from "./helpers/math-utils.js";
import defer * as attributedDeferredMath from "./helpers/math-utils.js" with { };
import defer * as deferredConfig from "./helpers/config.json" with { type: "json" };
import defer * as deferredTLA from "./helpers/deferred-tla.js";
import defer * as deferredAwaitText from "./helpers/deferred-await-text.js";
import "./helpers/side-effect-marker.js";

describe("basic import", () => {
  test("import function from local module", () => {
    expect(localFunction()).toBe(42);
  });

  test("import value from local module", () => {
    expect(localValue).toBe("hello");
  });

  test("import defer namespace syntax binds deferred module namespaces", () => {
    expect(deferredMath.add(2, 3)).toBe(5);
    expect(attributedDeferredMath.multiply(3, 4)).toBe(12);
  });

  test("import defer preserves attributes for asset modules", () => {
    expect(deferredConfig.name).toBe("goccia-test");
    expect(deferredConfig.default.name).toBe("goccia-test");
  });

  test("import defer eagerly evaluates top-level await dependencies", () => {
    expect(globalThis.__gocciaDeferredTLAOrder).toEqual([
      "tla start",
      "tla end",
    ]);
    expect(deferredTLA.value).toBe(23);
    expect(globalThis.__gocciaDeferredTLAOrder).toEqual([
      "tla start",
      "tla end",
    ]);
  });

  test("import defer ignores await text in comments and strings", () => {
    expect(globalThis.__gocciaDeferredAwaitTextEvaluated).toBeUndefined();
    expect(deferredAwaitText.value).toBe(31);
    expect(globalThis.__gocciaDeferredAwaitTextEvaluated).toBe(true);
  });

  test("side-effect imports evaluate their dependency", () => {
    expect(globalThis.moduleSideEffectImportCount).toBe(1);
  });
});

import { add, multiply } from "./helpers/math-utils.js";

describe("import from subdirectory", () => {
  test("import add from math-utils", () => {
    expect(add(2, 3)).toBe(5);
  });

  test("import multiply from math-utils", () => {
    expect(multiply(4, 5)).toBe(20);
  });
});

import { greet as sayHello } from "./helpers/string-utils.js";

describe("aliased import", () => {
  test("import with alias", () => {
    expect(sayHello("World")).toBe("Hello, World!");
  });
});
