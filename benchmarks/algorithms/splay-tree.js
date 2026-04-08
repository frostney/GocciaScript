/*---
description: Splay tree benchmarks — adapted from Octane 2.0 Splay benchmark
---*/

// Splay tree node
class SplayNode {
  constructor(key, value) {
    this.key = key;
    this.value = value;
    this.left = null;
    this.right = null;
  }
}

// Splay tree with insert, find, splay, and delete via recursion
class SplayTree {
  constructor() {
    this.root = null;
    this.size = 0;
  }

  // Splay the node with the given key to the root
  splay(key) {
    if (this.root === null) { return; }
    this.root = this.#splayStep(this.root, key);
  }

  #splayStep(node, key) {
    if (node === null) { return null; }
    if (key === node.key) { return node; }
    if (key < node.key) {
      if (node.left === null) { return node; }
      if (key < node.left.key) {
        // Zig-zig: rotate right twice
        node.left.left = this.#splayStep(node.left.left, key);
        node = this.#rotateRight(node);
      } else if (key > node.left.key) {
        // Zig-zag: rotate left child left, then rotate right
        node.left.right = this.#splayStep(node.left.right, key);
        if (node.left.right !== null) {
          node.left = this.#rotateLeft(node.left);
        }
      }
      return node.left === null ? node : this.#rotateRight(node);
    } else {
      if (node.right === null) { return node; }
      if (key > node.right.key) {
        // Zag-zag: rotate left twice
        node.right.right = this.#splayStep(node.right.right, key);
        node = this.#rotateLeft(node);
      } else if (key < node.right.key) {
        // Zag-zig: rotate right child right, then rotate left
        node.right.left = this.#splayStep(node.right.left, key);
        if (node.right.left !== null) {
          node.right = this.#rotateRight(node.right);
        }
      }
      return node.right === null ? node : this.#rotateLeft(node);
    }
  }

  #rotateRight(node) {
    const temp = node.left;
    node.left = temp.right;
    temp.right = node;
    return temp;
  }

  #rotateLeft(node) {
    const temp = node.right;
    node.right = temp.left;
    temp.left = node;
    return temp;
  }

  insert(key, value) {
    if (this.root === null) {
      this.root = new SplayNode(key, value);
      this.size = this.size + 1;
      return;
    }
    this.splay(key);
    if (this.root.key === key) {
      this.root.value = value;
      return;
    }
    const node = new SplayNode(key, value);
    if (key < this.root.key) {
      node.left = this.root.left;
      node.right = this.root;
      this.root.left = null;
    } else {
      node.right = this.root.right;
      node.left = this.root;
      this.root.right = null;
    }
    this.root = node;
    this.size = this.size + 1;
  }

  find(key) {
    if (this.root === null) { return null; }
    this.splay(key);
    return this.root.key === key ? this.root.value : null;
  }

  remove(key) {
    if (this.root === null) { return; }
    this.splay(key);
    if (this.root.key !== key) { return; }
    if (this.root.left === null) {
      this.root = this.root.right;
    } else {
      const right = this.root.right;
      this.root = this.root.left;
      this.splay(key);
      this.root.right = right;
    }
    this.size = this.size - 1;
  }
}

// Generate a pseudo-random sequence of keys for deterministic benchmarks
const generateKeys = (count, seed) => {
  let s = seed;
  return Array.from({ length: count }, () => {
    s = (s * 1103515245 + 12345) & 0x7fffffff;
    return s;
  });
};

suite("splay tree", () => {
  bench("insert 1000 keys", {
    setup: () => generateKeys(1000, 42),
    run: (keys) => {
      const tree = new SplayTree();
      for (const key of keys) {
        tree.insert(key, key);
      }
    },
  });

  bench("insert + find 500 keys", {
    setup: () => generateKeys(1000, 42),
    run: (keys) => {
      const tree = new SplayTree();
      for (const key of keys) {
        tree.insert(key, key);
      }
      const searchKeys = keys.slice(0, 500);
      for (const key of searchKeys) {
        tree.find(key);
      }
    },
  });

  bench("mixed insert/delete 500 cycles", {
    setup: () => generateKeys(1000, 42),
    run: (keys) => {
      const tree = new SplayTree();
      for (const key of keys) {
        tree.insert(key, key);
      }
      const half = keys.slice(0, 500);
      for (const key of half) {
        tree.remove(key);
      }
      for (const key of half) {
        tree.insert(key, key);
      }
    },
  });

  bench("large scale 3000 keys", {
    setup: () => generateKeys(3000, 99),
    run: (keys) => {
      const tree = new SplayTree();
      for (const key of keys) {
        tree.insert(key, key);
      }
      for (const key of keys) {
        tree.find(key);
      }
    },
  });
});
