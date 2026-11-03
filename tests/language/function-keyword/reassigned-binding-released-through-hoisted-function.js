/*---
description: >
  A hoisted function declaration that names a later top-level let or var
  binding reads the global binding (ES2026 §9.1.1.4 Global Environment
  Records), not a register of the script's frame: once the script assigns the
  binding again, its initializer is unreachable and a collection reclaims it.
features: [function-keyword, let, var, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

const measure = (drop) => {
  Goccia.gc();
  const before = Goccia.gc.bytesAllocated;
  drop();
  Goccia.gc();
  return before - Goccia.gc.bytesAllocated;
};

function readLexical() {
  return lexical;
}

function readVar() {
  return variable;
}

let lexical = new ArrayBuffer(SIZE);
const lexicalFreed = measure(() => {
  lexical = null;
});

var variable = new ArrayBuffer(SIZE);
const varFreed = measure(() => {
  variable = null;
});

describe("a reassigned top-level binding a hoisted function names", () => {
  test("releases a let initializer", () => {
    expect(lexicalFreed).toBeGreaterThanOrEqual(SIZE);
    expect(readLexical()).toBe(null);
  });

  test("releases a var initializer", () => {
    expect(varFreed).toBeGreaterThanOrEqual(SIZE);
    expect(readVar()).toBe(null);
  });
});
