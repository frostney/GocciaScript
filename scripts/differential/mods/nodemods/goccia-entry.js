// The goccia-only half of the fixture. Both behaviours here are deliberate
// deviations from Node (and from bun), so no external oracle can gate them.
//
// The dynamic import lives here rather than in the differential suite because
// a bare specifier resolves from the importing file's directory: only a module
// inside this directory walks up into the committed `node_modules/` next door.
export { modfieldLabel } from "modfield";

export const importCommonJSOnlyPackage = () => import("cjsonly");
