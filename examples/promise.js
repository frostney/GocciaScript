// Promise examples for GocciaScript
// Demonstrates that .then()/.catch()/.finally() callbacks fire
// after synchronous code completes (via the microtask queue).

console.log("=== Basic Promise.resolve ===");
console.log("1. before");
Promise.resolve(42).then((v) => console.log("3. resolved:", v));
console.log("2. after (sync runs first)");

console.log("");
console.log("=== Promise chaining ===");
Promise.resolve(1)
  .then((v) => {
    console.log("step 1:", v);
    return v + 1;
  })
  .then((v) => {
    console.log("step 2:", v);
    return v * 10;
  })
  .then((v) => {
    console.log("step 3:", v);
  });

console.log("");
console.log("=== Error handling with catch ===");
Promise.reject("something went wrong")
  .then((v) => console.log("this won't run"))
  .catch((e) => {
    console.log("caught:", e);
    return "recovered";
  })
  .then((v) => console.log("after recovery:", v));

console.log("");
console.log("=== finally ===");
Promise.resolve("hello")
  .finally(() => console.log("finally runs (fulfilled)"))
  .then((v) => console.log("value preserved:", v));

Promise.reject("oops")
  .finally(() => console.log("finally runs (rejected)"))
  .catch((e) => console.log("reason preserved:", e));

console.log("");
console.log("=== Promise constructor ===");
const p = new Promise((resolve, reject) => {
  console.log("executor runs synchronously");
  resolve("from constructor");
});
p.then((v) => console.log("constructor result:", v));

console.log("");
console.log("=== Promise.all ===");
Promise.all([
  Promise.resolve("a"),
  Promise.resolve("b"),
  Promise.resolve("c")
]).then((values) => console.log("all resolved:", values));

console.log("");
console.log("=== Promise.race ===");
Promise.race([
  Promise.resolve("winner"),
  Promise.resolve("loser")
]).then((v) => console.log("race winner:", v));

console.log("");
console.log("=== Promise.any ===");
Promise.any([
  Promise.reject("err1"),
  Promise.resolve("success"),
  Promise.reject("err2")
]).then((v) => console.log("any first success:", v));

console.log("");
console.log("=== Promise.allSettled ===");
Promise.allSettled([
  Promise.resolve(1),
  Promise.reject("fail"),
  Promise.resolve(3)
]).then((results) => {
  results.forEach((r) => {
    if (r.status === "fulfilled") {
      console.log("  fulfilled:", r.value);
    } else {
      console.log("  rejected:", r.reason);
    }
  });
});

console.log("");
console.log("=== Nested promise resolution ===");
Promise.resolve("outer")
  .then((v) => {
    console.log("outer:", v);
    return Promise.resolve("inner").then((v2) => {
      console.log("inner:", v2);
      return "done";
    });
  })
  .then((v) => console.log("final:", v));
