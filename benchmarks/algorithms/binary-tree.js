/*---
description: Binary tree benchmarks — adapted from the Computer Language Benchmarks Game
---*/

const buildTree = (depth) => {
  if (depth === 0) {
    return { left: null, right: null, value: 1 };
  }
  return {
    left: buildTree(depth - 1),
    right: buildTree(depth - 1),
    value: depth,
  };
};

const checkTree = (node) => {
  if (node.left === null) {
    return node.value;
  }
  return node.value + checkTree(node.left) + checkTree(node.right);
};

const buildAndCheck = (depth) => {
  const tree = buildTree(depth);
  return checkTree(tree);
};

suite("binary tree", () => {
  bench("build depth 10 (1023 nodes)", {
    run: () => {
      buildTree(10);
    },
  });

  bench("build+check depth 10", {
    run: () => {
      buildAndCheck(10);
    },
  });

  bench("build+check depth 12 (4095 nodes)", {
    run: () => {
      buildAndCheck(12);
    },
  });

  bench("GC stress: repeated build+check depth 10 (x5)", {
    run: () => {
      buildAndCheck(10);
      buildAndCheck(10);
      buildAndCheck(10);
      buildAndCheck(10);
      buildAndCheck(10);
    },
  });
});
