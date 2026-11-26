// new.target of a constructor frame with a rest parameter or an arguments object survives the collection the
// rest array or the arguments object starts (the class is young: created just before)
let ok = 0, n = 0;
for (let round = 0; round < 20000; round++) {
  class Base { constructor(a, b) { this.a = a; this.b = b; } }
  class Rest extends Base { constructor(...args) { super(...args); } tag() { return 1; } }
  class Args extends Base { constructor() { super(arguments[0], arguments[1]); } tag() { return 2; } }
  const pad = [round, round + 1, round + 2, round + 3, round + 4, round + 5, round + 6, round + 7];
  const r = new Rest(round, pad, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
  const g = new Args(round, pad, 1, 2, 3, 4, 5, 6, 7, 8);
  n += 2;
  if (Object.getPrototypeOf(r) === Rest.prototype && r.tag() === 1 && r.a === round) ok++;
  if (Object.getPrototypeOf(g) === Args.prototype && g.tag() === 2 && g.a === round) ok++;
}
console.log(ok === n ? 'ok' : 'FAIL ' + (n - ok));
