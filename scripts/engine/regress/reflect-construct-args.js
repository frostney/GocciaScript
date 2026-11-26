// Reflect.construct passes its argument list as a heap array's elements: a base class's [[Construct]] reads the
// prototype, makes the new object and runs the field initializer (allocations) and must copy the arguments to the
// value stack before them (they were copied after: a collection had moved the array, its old place read)
let bad = 0;
class C {
  f = [1, 2, 3];
  constructor(a, b) { this.a = a; this.b = b; }
}
for (let k = 0; k < 3000; k++) {
  const o = Reflect.construct(C, [{ k }, [k, k]]);
  if (o.a.k !== k || o.b[1] !== k || o.f.length !== 3) { bad++; if (bad < 5) console.log("FAIL", k); }
}
console.log(bad === 0 ? "ok" : "bad " + bad);
