/*---
description: type-only module syntax is parsed correctly and preserves runtime behavior
features: [types-as-comments]
---*/

import { runtimeValue, type MissingValueType } from "./helpers/type-only-side-effect.js";
import { type PhantomType } from "./helpers/type-only-only-side-effect.js";
import { type as kind } from "./helpers/type-value-export.js";
import { value } from "./helpers/mixed-type-export.js";
import { kind as exportedKind } from "./helpers/type-renamed-export.js";
import { answer } from "./helpers/exported-interface.js";
import type * as MissingNamespace from "./helpers/nonexistent-namespace.js";

// None of these modules exists. Each statement is erased whole, so this file
// loads without trying to resolve any of them.
export type { MissingValueType } from "./helpers/nonexistent-re-export.js";
export type * from "./helpers/nonexistent-star.js";
export type * as MissingStar from "./helpers/nonexistent-star.js";

test("import type is skipped", () => {
  import type { Foo } from './nonexistent.js';
  const x = 42;
  expect(x).toBe(42);
});

test("export type is skipped", () => {
  export type { Bar };
  const y = "hello";
  expect(y).toBe("hello");
});

test("mixed import keeps runtime bindings and skips type-only bindings", () => {
  expect(runtimeValue).toBe(7);
});

test("type-only named import still loads the module at runtime", () => {
  expect(globalThis.__typeOnlyNamedImportLoaded).toBe(1);
});

test("mixed export keeps value exports and skips type-only bindings", () => {
  expect(value).toBe(1);
});

test("type named binding can still be imported as a value", () => {
  expect(kind).toBe(7);
});

test("type named export can still be re-exported as a value binding", () => {
  expect(exportedKind).toBe(9);
});

test("export interface is skipped while value exports remain available", () => {
  expect(answer).toBe(42);
});

test("type-only namespace imports and type-only re-exports load nothing", () => {
  expect(typeof MissingNamespace).toBe("undefined");
  expect(typeof MissingStar).toBe("undefined");
});

test("type declarations in a block or a switch case bind nothing", () => {
  {
    type InBlock = number;
    interface AlsoInBlock {}
    expect(typeof InBlock).toBe("undefined");
    expect(typeof AlsoInBlock).toBe("undefined");
  }
  const kind = 1;
  switch (kind) {
    case 1:
      type InCase = string;
      expect(typeof InCase).toBe("undefined");
      break;
  }
});
