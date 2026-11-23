// g2-flags: --gc-stress
// an optimized region with inlined callees keeps their registers past its frame's (ORegs past MaxRegs): StackTop
// must cover them while the region runs, or a collection in an embedded instruction (a BigInt multiply allocates)
// skips them, and the BigInts they hold are read stale after it (a later deopt wrote them back into the frame)
var Big = BigInt, NumberIsFinite = Number.isFinite, MathAbs = Math.abs;
var nsPerDay = 86400000000000n, maxTimeDuration = 2n ** 53n * 1000000000n - 1n;
function babs(n) { return n < 0n ? -n : n; }
function bigOf(n) { return Big(n); }
function TDC(h, mi, s, ms, us, ns) {
  return bigOf(h) * 3600000000000n + bigOf(mi) * 60000000000n + bigOf(s) * 1000000000n + bigOf(ms) * 1000000n +
    bigOf(us) * 1000n + bigOf(ns);
}
function IsValidDuration(y, mo, w, d, h, mi, s, ms, us, ns) {
  var v = [y, mo, w, d, h, mi, s, ms, us, ns], sg = 0;
  for (var i = 0; i < 10; i++) {
    if (!NumberIsFinite(v[i])) return false;
    if (v[i] < 0) { if (sg > 0) return false; sg = -1; }
    else if (v[i] > 0) { if (sg < 0) return false; sg = 1; }
  }
  if (MathAbs(y) >= 4294967296 || MathAbs(mo) >= 4294967296 || MathAbs(w) >= 4294967296) return false;
  var total = bigOf(d) * nsPerDay + TDC(h, mi, s, ms, us, ns);
  if (babs(total) > maxTimeDuration) return false;
  return true;
}
function Create(y, mo, w, d, h, mi, s, ms, us, ns) {
  if (!IsValidDuration(y, mo, w, d, h, mi, s, ms, us, ns)) throw new RangeError("invalid");
  return { years: y + 0, months: mo + 0, weeks: w + 0, days: d + 0, hours: h + 0, minutes: mi + 0,
    seconds: s + 0, milliseconds: ms + 0, microseconds: us + 0, nanoseconds: ns + 0 };
}
var n = 0;
for (var i = 0; i < 3000; i++) { var r = Create(0, 0, 0, i % 300, 0, 0, 0, 0, 0, 0); n += r.days; }
console.log(n === 448500 ? "ok" : "bad " + n);
