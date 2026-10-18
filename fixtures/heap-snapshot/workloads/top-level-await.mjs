// Heap snapshot workload: a module paused by a top-level await. The module
// body's own continuation is what the snapshot holds.

const items = [1, 2, 3].map((n) => ({ n, sq: n * n }));
let total = 0;
for (const it of items) total += it.sq;
print("before", total);
const answer = await hostCall("x", 1);
total += answer;
print("after", total, items.length);
