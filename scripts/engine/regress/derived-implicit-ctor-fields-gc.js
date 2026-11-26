// a derived class with an implicit constructor and fields: the object its parent constructed stays valid when
// defining a field (an overflow slot) starts a collection that moves it (run with a small nursery)
class Base { a = 1; b = 2; c = 3; }
class Derived extends Base { d = this.a + this.b; e = 5; f = 6; g = 7; h = 8; }
let ok = 0, n = 0;
const keep = [];
for (let round = 0; round < 40000; round++) {
  const o = new Derived();
  const pad = { x: round, y: [round, round + 1, round + 2] };
  if ((round & 255) === 0) keep.push(pad);
  n++;
  if (o.d === 3 && o.h === 8 && o.c === 3) ok++;
}
console.log(ok === n ? 'ok' : 'FAIL ' + (n - ok));
