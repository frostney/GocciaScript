// Differential suite J — TypeScript's `.js`-specifier ESM convention.
//
// TypeScript-authored ESM writes the *emitted* specifier (`./dep.js`) while the
// file on disk is the TypeScript input (`dep.ts`). tsx, ts-node, vite and bun
// all fall back from the JavaScript extension to the TypeScript one when the
// literal path misses. The fallback must never shadow a real file, so a `.js`
// that exists on disk always wins over its `.ts` neighbour.
import { plain } from "./mods/tsspec-plain.js";
import { reexported } from "./mods/tsspec-reexport.js";
import { fromTsx } from "./mods/tsspec-jsx.js";
import { fromTsx as fromTsxViaJsx } from "./mods/tsspec-jsx.jsx";
import { fromMts } from "./mods/tsspec-esm.mjs";
import { shadowed } from "./mods/tsspec-shadow.js";

describe("TypeScript .js specifier resolution", () => {
  test(".js specifier resolves to the .ts file on disk", () => {
    expect(plain).toBe("ts");
  });

  test("the fallback applies transitively, not just at the entry file", () => {
    expect(reexported).toBe("ts");
  });

  test(".js specifier resolves to a .tsx file when that is what exists", () => {
    expect(fromTsx).toBe("tsx");
  });

  test(".jsx specifier resolves to the TypeScript file", () => {
    expect(fromTsxViaJsx).toBe("tsx");
  });

  test(".mjs specifier resolves to the .mts file", () => {
    expect(fromMts).toBe("mts");
  });

  test("a real .js file wins over its same-stem .ts neighbour", () => {
    expect(shadowed).toBe("js");
  });
});
