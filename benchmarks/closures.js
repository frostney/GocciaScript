/*---
description: Function and closure benchmarks
---*/

suite("closure capture", () => {
  bench("closure over single variable", {
    run: () => {
      const makeCounter = () => {
        let count = 0;
        return () => {
          count = count + 1;
          return count;
        };
      };
      const counter = makeCounter();
      counter();
      counter();
      counter();
    },
  });

  bench("closure over multiple variables", {
    run: () => {
      const makeAdder = (a, b, c) => {
        return (x) => a + b + c + x;
      };
      const add = makeAdder(1, 2, 3);
      const r1 = add(10);
      const r2 = add(20);
    },
  });

  bench("nested closures", {
    run: () => {
      const outer = (x) => {
        return (y) => {
          return (z) => x + y + z;
        };
      };
      const f = outer(1)(2);
      const r = f(3);
    },
  });
});

suite("higher-order functions", () => {
  bench("function as argument", {
    run: () => {
      const apply = (fn, x) => fn(x);
      const double = (x) => x * 2;
      const r1 = apply(double, 5);
      const r2 = apply(double, 10);
      const r3 = apply(double, 15);
    },
  });

  bench("function returning function", {
    run: () => {
      const multiplier = (factor) => (x) => x * factor;
      const triple = multiplier(3);
      const r1 = triple(1);
      const r2 = triple(2);
      const r3 = triple(3);
    },
  });

  bench("compose two functions", {
    run: () => {
      const compose = (f, g) => (x) => f(g(x));
      const inc = (x) => x + 1;
      const double = (x) => x * 2;
      const incThenDouble = compose(double, inc);
      const r1 = incThenDouble(3);
      const r2 = incThenDouble(7);
    },
  });
});

suite("call/apply/bind", () => {
  bench("fn.call", {
    run: () => {
      const greet = (greeting) => greeting + " " + "world";
      const r1 = greet.call(null, "hello");
      const r2 = greet.call(null, "hi");
    },
  });

  bench("fn.apply", {
    run: () => {
      const sum = (a, b, c) => a + b + c;
      const r1 = sum.apply(null, [1, 2, 3]);
      const r2 = sum.apply(null, [4, 5, 6]);
    },
  });

  bench("fn.bind", {
    run: () => {
      const add = (a, b) => a + b;
      const add5 = add.bind(null, 5);
      const r1 = add5(10);
      const r2 = add5(20);
    },
  });
});

suite("recursion", () => {
  bench("recursive sum to 50", {
    run: () => {
      const sum = (n) => n <= 0 ? 0 : n + sum(n - 1);
      const r = sum(50);
    },
  });

  bench("recursive tree traversal", {
    run: () => {
      const node = (v, l, r) => ({ value: v, left: l, right: r });
      const tree = node(1,
        node(2, node(4, null, null), node(5, null, null)),
        node(3, node(6, null, null), node(7, null, null))
      );
      const sumTree = (n) => {
        if (n === null) { return 0; }
        return n.value + sumTree(n.left) + sumTree(n.right);
      };
      const r = sumTree(tree);
    },
  });
});
