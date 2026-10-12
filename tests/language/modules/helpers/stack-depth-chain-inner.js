// Counts the nested calls this module's top level can make before the
// RangeError (imported-module-stack-depth.js).
let calls = 0;
const recurse = () => {
  calls++;
  recurse();
};
let error;
try {
  recurse();
} catch (e) {
  error = e;
}

export { calls, error };
