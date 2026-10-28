/*---
description: import type binds `type` as the default import unless a binding, "{" or "*" follows
features: [types-as-comments]
---*/

import { boundType as semicolonType } from "./helpers/type-default-import.js";
import {
  boundType as namedType,
  boundY as namedY,
} from "./helpers/type-default-named-import.js";
import {
  boundType as namespaceType,
  boundY as namespaceY,
} from "./helpers/type-default-namespace-import.js";
import { boundType as escapedType } from "./helpers/type-default-escaped-import.js";
import {
  boundType as namespaceFromType,
  boundY as namespaceFromY,
} from "./helpers/type-default-namespace-from-alias.js";
import {
  boundType as namedFromType,
  boundY as namedFromY,
} from "./helpers/type-default-named-from-alias.js";
import { kinds } from "./helpers/type-only-from-binding.js";

describe("import type with a default binding named type", () => {
  test("import type from \"m\" binds the default export as type", () => {
    expect(semicolonType).toBe("the default");
  });

  test("import type, { y } from \"m\" binds type and the named import", () => {
    expect(namedType).toBe("the default");
    expect(namedY).toBe("named y");
  });

  test("import type, * as ns from \"m\" binds type and the namespace", () => {
    expect(namespaceType).toBe("the default");
    expect(namespaceY).toBe("named y");
  });

  test("a namespace alias may be a contextual keyword such as from", () => {
    expect(namespaceFromType).toBe("the default");
    expect(namespaceFromY).toBe("named y");
  });

  test("a named import alias may be a contextual keyword such as from", () => {
    expect(namedFromType).toBe("the default");
    expect(namedFromY).toBe("named y");
  });

  test("an escaped type stays a plain default binding", () => {
    expect(escapedType).toBe("the default");
  });
});

describe("import type that stays type-only", () => {
  test("a default binding named from, a default binding, a member list and a namespace bind nothing and load nothing", () => {
    expect(kinds).toEqual([
      "undefined",
      "undefined",
      "undefined",
      "undefined",
      "undefined",
    ]);
  });
});
