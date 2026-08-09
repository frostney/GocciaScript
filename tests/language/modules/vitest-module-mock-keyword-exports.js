/*---
description: >
  A vi.mock factory may name its exports with words that cannot be bound by a
  `const` declaration — reserved words like `class`, and `await`, `yield`,
  `eval` and `arguments`, which module code cannot bind at all. Vitest exposes
  every one of them as a module export, so GocciaScript does too, by binding
  the value to a generated local and exporting it under an alias. Verified
  against Vitest 4.1.10: a factory returning these keys makes them all readable
  through the namespace, and `export { local as await }` is accepted by Node
  and by GocciaScript alike.
features: [modules, runtime-modules]
---*/

import { describe, expect, test, vi } from "vitest";

import * as mocked from "./helpers/mock-keyword-export-target.js";
import * as generated from "./helpers/mock-generated-binding-target.js";

vi.mock("./helpers/mock-keyword-export-target.js", () => ({
  value: "MOCKED",
  class: 1,
  static: 2,
  import: 3,
  function: 4,
  await: 5,
  yield: 6,
  eval: 7,
  arguments: 8,
}));

// The shim declares a local for the factory result and one per aliased
// export. A factory may name its exports anything, including those generated
// names, which produced `const X = ...; export const X = X.X;` — a
// redeclaration that failed to parse. The names are now chosen against the
// factory's own key set.
vi.mock("./helpers/mock-generated-binding-target.js", () => ({
  value: "MOCKED",
  __gocciaMockFactoryResult: 1,
  __gocciaMockFactoryResultAlias1: 2,
  class: 3,
}));

describe("vi.mock factory keys that cannot be const bindings", () => {
  test("an ordinary key still exports normally alongside them", () => {
    expect(mocked.value).toBe("MOCKED");
  });

  test("reserved words are exported under their own names", () => {
    expect(mocked["class"]).toBe(1);
    expect(mocked["static"]).toBe(2);
    expect(mocked["import"]).toBe(3);
    expect(mocked["function"]).toBe(4);
  });

  test("names module code cannot bind are exported too", () => {
    // Not keywords, but barred as BindingIdentifiers in module source, which
    // is always strict: `export const await = …` is a SyntaxError in Node.
    expect(mocked["await"]).toBe(5);
    expect(mocked["yield"]).toBe(6);
    expect(mocked["eval"]).toBe(7);
    expect(mocked["arguments"]).toBe(8);
  });

  test("keys colliding with the shim's generated bindings still work", () => {
    expect(generated.value).toBe("MOCKED");
    expect(generated["__gocciaMockFactoryResult"]).toBe(1);
    expect(generated["__gocciaMockFactoryResultAlias1"]).toBe(2);
    // Present alongside an aliased export, whose local must also dodge the key.
    expect(generated["class"]).toBe(3);
  });
});
