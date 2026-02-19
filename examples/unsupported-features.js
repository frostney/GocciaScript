// This script demonstrates unsupported features in GocciaScript.
// Each construct below parses successfully and is treated as a no-op,
// with a warning emitted to stdout.

// --- Traditional loops (use array methods instead) ---

for (let i = 0; i < 5; i++) {
  console.log("This will NOT execute");
}

while (true) {
  console.log("This will NOT execute either");
}

do {
  console.log("Nor will this");
} while (false);

// The idiomatic GocciaScript alternative:
const numbers = [1, 2, 3, 4, 5];
numbers.forEach((n) => console.log("forEach:", n));

const doubled = numbers.map((n) => n * 2);
console.log("map:", doubled);

const evens = numbers.filter((n) => n % 2 === 0);
console.log("filter:", evens);

const sum = numbers.reduce((acc, n) => acc + n, 0);
console.log("reduce:", sum);

// --- with statement (no alternative) ---

with (Math) {
  console.log("This will NOT execute");
}

// --- Default imports/exports ---

import foo from "some-module";
import * as bar from "some-module";
import "some-side-effect";
export default 42;
export default class Unused {
  constructor() {
    this.x = 1;
  }
}

// Named imports/exports work as expected:
// import { add } from "./math.js";
// export const PI = 3.14159;
// export { add, multiply };

console.log("Script completed successfully despite unsupported features above.");
