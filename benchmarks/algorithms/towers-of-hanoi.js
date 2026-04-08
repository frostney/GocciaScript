/*---
description: Towers of Hanoi benchmarks — classic recursion stress test
---*/

const hanoi = (n, source, target, auxiliary) => {
  if (n === 0) { return 0; }
  const left = hanoi(n - 1, source, auxiliary, target);
  const right = hanoi(n - 1, auxiliary, target, source);
  return left + 1 + right;
};

suite("towers of hanoi", () => {
  bench("15 disks (32767 moves)", {
    run: () => {
      hanoi(15, "A", "C", "B");
    },
  });

  bench("17 disks (131071 moves)", {
    run: () => {
      hanoi(17, "A", "C", "B");
    },
  });

  bench("18 disks (262143 moves)", {
    run: () => {
      hanoi(18, "A", "C", "B");
    },
  });
});
