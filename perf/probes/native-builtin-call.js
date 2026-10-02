// Calls from bytecode into built-in functions, which cross the VM's native
// call boundary rather than staying in the dispatch loop: a function held in a
// local, a method on a namespace object, a string method and two array
// methods, with zero to three arguments. Nothing here allocates beyond the
// numbers the calls return, so the time is the call path itself.
__gocciaRegisterProbe({
  name: "native-builtin-call",
  run: (innerIterations) => {
    const abs = Math.abs;
    const text = "bytecode";
    const stack = [];
    let total = 0;
    for (let i = 0; i < innerIterations; i = i + 1) {
      total = total + abs(8 - (i & 15));
      total = total + Math.max(i & 7, 3, 5);
      total = total + text.charCodeAt(i & 7);
      stack.push(i & 3);
      total = total + stack.pop();
    }
    return total | 0;
  },
  // The default run's checksum was computed by Node.js; other sizes only
  // check that the result is an integer.
  verify: (checksum, innerIterations) =>
    innerIterations === 100000 ? checksum === 11675000 : checksum === (checksum | 0),
});
