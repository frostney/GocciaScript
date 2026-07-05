// This script lists syntax that GocciaScript rejects by default and shows
// supported alternatives. To observe the legacy warning/recovery behavior for
// these snippets, run a separate file with --warning-unsupported-features.

const unsupported = [
  {
    syntax: "var x = 42;",
    alternative: "let x = 42; or const x = 42;",
  },
  {
    syntax: "for (let i = 0; i < 5; i++) { ... }",
    alternative: "array methods or for...of",
  },
  {
    syntax: "while (condition) { ... }",
    alternative: "for...of, array methods, or --compat-while-loops",
  },
  {
    syntax: "with (object) { ... }",
    alternative: "explicit property access",
  },
];

unsupported.forEach((entry) => {
  console.log("Unsupported by default:", entry.syntax);
  console.log("  Prefer:", entry.alternative);
});

const numbers = [1, 2, 3, 4, 5];
numbers.forEach((n) => console.log("forEach:", n));

const doubled = numbers.map((n) => n * 2);
console.log("map:", doubled);

const evens = numbers.filter((n) => n % 2 === 0);
console.log("filter:", evens);

const sum = numbers.reduce((acc, n) => acc + n, 0);
console.log("reduce:", sum);

console.log("Script completed successfully with supported syntax.");
