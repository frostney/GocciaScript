/*---
description: import type with a default binding named type binds without a semicolon, and the type-only forms stay erased
features: [automatic-semicolon-insertion, types-as-comments]
---*/

import { boundType as inlineType } from "./helpers/type-default-import-asi.js"
import { boundType as lineBreakType } from "./helpers/type-default-import-asi-line-break.js"
import {
  boundType as namedType,
  boundY as namedY,
} from "./helpers/type-default-named-import.js"
import {
  boundType as namespaceType,
  boundY as namespaceY,
} from "./helpers/type-default-namespace-import.js"
import { kinds } from "./helpers/type-only-from-binding.js"

describe("ASI import type with a default binding named type", () => {
  test("import type from \"m\" without a semicolon binds the default export as type", () => {
    expect(inlineType).toBe("the default")
  })

  test("a line break between type and from does not change the binding", () => {
    expect(lineBreakType).toBe("the default")
  })

  test("import type, { y } from \"m\" without a semicolon binds type and y", () => {
    expect(namedType).toBe("the default")
    expect(namedY).toBe("named y")
  })

  test("import type, * as ns from \"m\" without a semicolon binds type and ns", () => {
    expect(namespaceType).toBe("the default")
    expect(namespaceY).toBe("named y")
  })

  test("the type-only forms without a semicolon bind nothing and load nothing", () => {
    expect(kinds).toEqual(["undefined", "undefined", "undefined", "undefined", "undefined"])
  })
})
