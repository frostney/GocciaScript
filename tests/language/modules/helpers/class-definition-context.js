// A class whose field initializers reach for module-level host services. It
// lives here rather than in the importing test so that the defining file and
// the constructing file differ: `import()` and `import.meta` in a field
// initializer must resolve against this file, whichever module runs the
// construction and by whichever route.
export class DefinitionContext {
  meta = import.meta.url;
  dep = import("./math-utils.js");
}

export const definingUrl = import.meta.url;
