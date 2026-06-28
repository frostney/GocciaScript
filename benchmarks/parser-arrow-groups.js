/*---
description: >
  Arrow- and parenthesized-group-dense code. Exercises the parser's
  arrow-vs-parenthesized disambiguation (TGocciaParser.IsArrowFunction) and the
  parenthesized-expression path, which speculatively scan `(...)` groups. The
  per-file lex/parse timing is the signal for parenthesized-group re-lexing
  (issue #808); the run bodies keep it a valid execution benchmark too.
---*/

suite("arrow groups", () => {
  // Parenthesized-parameter arrows and parenthesized expressions: every group
  // is probed by IsArrowFunction before the real parse.
  bench("parenthesized-parameter arrows", {
    run: () => {
      const add = (a, b) => (a + b);
      const mul = (a, b, c) => (a * b) * (c - a);
      const mix = (a, b, c, d) => ((a + b) * (c - d)) - ((a - b) * (c + d));
      const r1 = add(1, 2);
      const r2 = mul(3, 4, 5);
      const r3 = mix(1, 2, 3, 4);
      return (r1 + r2) - r3;
    },
  });

  // Curried / nested arrows: each `(` opens a fresh disambiguation.
  bench("curried arrows", {
    run: () => {
      const curry = (a) => (b) => (c) => (d) => (((a + b) + c) + d);
      const step = curry(1)(2)(3);
      const r1 = step(4);
      const r2 = curry(10)(20)(30)(40);
      return r1 + r2;
    },
  });

  // Callback-dense chains: parenthesized arrow arguments inside calls.
  bench("callback chains", {
    run: () => {
      const xs = [1, 2, 3, 4, 5, 6, 7, 8];
      const out = xs
        .map((x) => (x + 1))
        .filter((x) => ((x % 2) === 0))
        .map((x) => ((x * x) - 1))
        .reduce((acc, x) => (acc + x), 0);
      return out;
    },
  });

  // Parenthesized expressions that are NOT arrows: IsArrowFunction scans the
  // whole group and fails, then the real parse re-reads it.
  bench("non-arrow parenthesized expressions", {
    run: () => {
      const a = 3, b = 4, c = 5, d = 6;
      const v1 = ((a + b) * (c + d));
      const v2 = (((a - b) + (c - d)) * ((a + c) - (b + d)));
      const v3 = ((((a))));
      return (v1 + v2) - v3;
    },
  });

  // Moderate nested grouping: the overlapping-scan case that grows
  // super-linearly when each group is re-lexed.
  bench("nested groups", {
    run: () => {
      const f = (n) => ((((((n + 1) - 2) + 3) - 4) + 5) - 6);
      const g = (n) => (((n * 2) + ((n * 3) - (n * 4))) + (((n) - n) + n));
      return f(10) + g(7);
    },
  });
});
