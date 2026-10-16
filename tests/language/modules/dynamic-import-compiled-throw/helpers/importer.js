// Module-level async function that evaluates import(), so the VM's dynamic
// import handling surfaces the imported module's rejection.
export async function importThrowing() {
  return import("./top-level-throw.js");
}
