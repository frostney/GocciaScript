// A `.js` specifier inside a TypeScript module, so the fallback has to work
// transitively and not just from the entry file.
export { plain as reexported } from "./tsspec-plain.js";
