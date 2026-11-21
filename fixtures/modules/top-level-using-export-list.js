// An export list that precedes the declarations it exports.
export { resource, asyncResource, constant };

using resource = null;
await using asyncResource = null;
const constant = "constant";

globalThis.topLevelUsingExportListEvaluated = true;
