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
});
