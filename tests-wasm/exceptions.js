// Expected: caught: oops
// Expected: done
try {
  throw new Error("oops");
} catch (e) {
  console.log("caught: " + e.message);
}
console.log("done");
