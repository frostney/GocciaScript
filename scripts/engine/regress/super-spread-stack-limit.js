// super(...args) copies the arguments to the value stack past the call's registers: more than the stack holds is a
// RangeError, as a spread call's is (Execute's opSuperCallV wrote past the end of the value stack's mapping)
class B { constructor(...a) { this.n = a.length; } }
class D extends B { constructor(arr) { super(...arr); } }
const arr = new Array(5000000).fill(0);
let r;
try { r = "returned " + new D(arr).n; } catch (e) { r = e instanceof RangeError ? "ok" : "threw " + e; }
const small = new D([1, 2, 3]).n === 3;
console.log(small ? r : "bad small " + small);
