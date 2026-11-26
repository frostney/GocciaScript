// a rest parameter of tens of thousands of arguments: its elements store is too big for the nursery and is made in
// old space, then filled from the arguments without a write barrier; it must be remembered from the start (the young
// arguments it holds were otherwise missed by the next minor collection)
let bad = 0;
const rest = (...r) => r;
for (let round = 0; round < 6; round++) {
  const big = Array.from({ length: 30000 }, (_, i) => ({ i, round }));
  const r = rest(...big);
  const more = Array.from({ length: 2000 }, (_, i) => [i]);
  let sum = 0;
  for (let i = 0; i < r.length; i += 997) sum += r[i].i + r[i].round;
  let want = 0;
  for (let i = 0; i < 30000; i += 997) want += i + round;
  if (sum !== want || r.length !== 30000 || r[29999].i !== 29999 || more.length !== 2000) bad++;
}
console.log(bad === 0 ? "ok" : "bad " + bad);
