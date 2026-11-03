/*---
description: A function declaration at a module's top level reads the module's top-level using bindings, which bind as const declarations do (ES2027 §14.3.1.2)
features: [explicit-resource-management, modules]
---*/

import readDefault, { readResource } from "../../../../fixtures/modules/top-level-using-functions.js";

globalThis.entryResource = { name: "global" };

function readEntry() {
  return entryResource.name;
}

using entryResource = {
  name: "entry",
  [Symbol.dispose]() {},
};

describe("function declarations and a top-level using binding", () => {
  test("an entry module's function reads its binding, not a global", () => {
    expect(readEntry()).toBe("entry");
  });

  test("an imported module's default export function reads its binding", () => {
    expect(readDefault()).toBe("imported");
  });

  test("an imported module's exported function reads its binding", () => {
    expect(readResource()).toBe("imported");
  });
});
