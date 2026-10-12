// Counts the nested calls this module's top level can make before the
// RangeError, before its await and after it (imported-module-stack-depth.js).
let calls = 0;
const recurse = () => {
  calls++;
  recurse();
};

let errorBeforeAwait;
try {
  recurse();
} catch (e) {
  errorBeforeAwait = e;
}
const callsBeforeAwait = calls;

await Promise.resolve();

calls = 0;
let errorAfterAwait;
try {
  recurse();
} catch (e) {
  errorAfterAwait = e;
}
const callsAfterAwait = calls;

export { callsBeforeAwait, errorBeforeAwait, callsAfterAwait, errorAfterAwait };
