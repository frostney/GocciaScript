// Exports a function that dynamically imports a sibling module.
// When called from a different directory, the import must still resolve
// relative to THIS file (the defining module), not the caller.
export const loadDep = () => import("./dynamic-import-dep.js");
