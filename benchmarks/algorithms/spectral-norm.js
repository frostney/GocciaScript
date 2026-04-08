/*---
description: Spectral norm benchmarks — adapted from the Computer Language Benchmarks Game
---*/

// A(i,j) = 1 / ((i+j)(i+j+1)/2 + i + 1)
const matrixA = (i, j) => 1 / ((i + j) * (i + j + 1) / 2 + i + 1);

// Multiply: v = A * u
const multiplyAv = (n, u) =>
  Array.from({ length: n }, (_, i) =>
    u.reduce((sum, uj, j) => sum + matrixA(i, j) * uj, 0)
  );

// Multiply: v = A^T * u
const multiplyAtv = (n, u) =>
  Array.from({ length: n }, (_, i) =>
    u.reduce((sum, uj, j) => sum + matrixA(j, i) * uj, 0)
  );

// Multiply: v = A^T * A * u
const multiplyAtAv = (n, u) => multiplyAtv(n, multiplyAv(n, u));

// Power iteration to compute spectral norm
const spectralNorm = (n) => {
  let u = Array.from({ length: n }, () => 1);
  let v = Array.from({ length: n }, () => 0);
  // 10 iterations of power method
  const iterations = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
  for (const _ of iterations) {
    v = multiplyAtAv(n, u);
    u = multiplyAtAv(n, v);
  }
  let vBv = 0;
  let vv = 0;
  for (const [i, ui] of u.entries()) {
    vBv = vBv + ui * v[i];
    vv = vv + v[i] * v[i];
  }
  return Math.sqrt(vBv / vv);
};

suite("spectral norm", () => {
  bench("N=30", {
    run: () => {
      spectralNorm(30);
    },
  });

  bench("N=50", {
    run: () => {
      spectralNorm(50);
    },
  });

  bench("N=80", {
    run: () => {
      spectralNorm(80);
    },
  });

  bench("single A*v multiply N=80", {
    setup: () => Array.from({ length: 80 }, () => 1),
    run: (u) => {
      multiplyAv(80, u);
    },
  });
});
