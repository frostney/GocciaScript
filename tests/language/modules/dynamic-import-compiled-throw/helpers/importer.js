// Interpreter-run (compat-function) async function that evaluates import() so
// the interpreter's TGocciaImportExpression ladder handles the rejection.
export async function importThrowing() {
  return import("./top-level-throw.js");
}
