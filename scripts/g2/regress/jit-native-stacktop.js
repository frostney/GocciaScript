// compiled code calling a native (G2Host.gc, a full collection) must not leave StackTop below its own registers: a
// dead register above the call's arguments (the loop's last class) was skipped by the collection, and scanned
// stale once a later frame (a deep super() chain) raised StackTop over it again
function f() {
  class Base { base = "base"; }
  let Leaf = Base;
  for (const _ of Array.from({ length: 3000 })) { const Parent = Leaf; Leaf = class extends Parent {}; }
  G2Host.gc();
  const instance = new Leaf();
  return instance.base === "base" && instance instanceof Base;
}
let ok = true;
for (let r = 0; r < 3; r++) ok = f() && ok;
console.log(ok ? "ok" : "bad");
