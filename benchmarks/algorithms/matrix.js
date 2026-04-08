/*---
description: Matrix operation benchmarks — inspired by SunSpider 3D math and Linpack
---*/

const createMatrix = (rows, cols, fill) =>
  Array.from({ length: rows }, (_, i) =>
    Array.from({ length: cols }, (_, j) => fill(i, j))
  );

const createIdentity = (n) =>
  createMatrix(n, n, (i, j) => i === j ? 1 : 0);

const multiply = (a, b) => {
  const rows = a.length;
  const cols = b[0].length;
  const inner = b.length;
  return Array.from({ length: rows }, (_, i) =>
    Array.from({ length: cols }, (_, j) =>
      a[i].reduce((sum, aik, k) => sum + aik * b[k][j], 0)
    )
  );
};

const transpose = (m) =>
  Array.from({ length: m[0].length }, (_, j) =>
    Array.from({ length: m.length }, (_, i) => m[i][j])
  );

// Recursive determinant via cofactor expansion
const determinant = (m) => {
  const n = m.length;
  if (n === 1) { return m[0][0]; }
  if (n === 2) { return m[0][0] * m[1][1] - m[0][1] * m[1][0]; }
  return m[0].reduce((sum, val, j) => {
    // Minor matrix: delete row 0, column j
    const minor = m.slice(1).map((row) =>
      [...row.slice(0, j), ...row.slice(j + 1)]
    );
    const sign = j % 2 === 0 ? 1 : -1;
    return sum + sign * val * determinant(minor);
  }, 0);
};

// Trace: sum of diagonal elements
const trace = (m) =>
  m.reduce((sum, row, i) => sum + row[i], 0);

// Frobenius norm: sqrt of sum of squares of all elements
const frobeniusNorm = (m) =>
  Math.sqrt(
    m.reduce((sum, row) =>
      sum + row.reduce((s, v) => s + v * v, 0), 0)
  );

suite("matrix operations", () => {
  bench("multiply 10x10", {
    setup: () => ({
      a: createMatrix(10, 10, (i, j) => (i + 1) * (j + 1) * 0.1),
      b: createMatrix(10, 10, (i, j) => (i - j) * 0.5 + 1),
    }),
    run: (data) => {
      multiply(data.a, data.b);
    },
  });

  bench("multiply 20x20", {
    setup: () => ({
      a: createMatrix(20, 20, (i, j) => Math.sin(i + j)),
      b: createMatrix(20, 20, (i, j) => Math.cos(i - j)),
    }),
    run: (data) => {
      multiply(data.a, data.b);
    },
  });

  bench("transpose 30x30", {
    setup: () => createMatrix(30, 30, (i, j) => i * 30 + j),
    run: (m) => {
      transpose(m);
    },
  });

  bench("determinant 6x6", {
    setup: () => createMatrix(6, 6, (i, j) => {
      const v = ((i * 7 + j * 13 + 3) % 19) - 9;
      return v * 0.1;
    }),
    run: (m) => {
      determinant(m);
    },
  });

  bench("trace + frobenius 20x20", {
    setup: () => createMatrix(20, 20, (i, j) => (i + 1) * (j + 1) * 0.01),
    run: (m) => {
      trace(m);
      frobeniusNorm(m);
    },
  });

  bench("multiply + transpose 15x15", {
    setup: () => ({
      a: createMatrix(15, 15, (i, j) => i + j),
      b: createMatrix(15, 15, (i, j) => i - j),
    }),
    run: (data) => {
      const c = multiply(data.a, data.b);
      transpose(c);
    },
  });
});
