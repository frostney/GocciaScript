// Expected: 8
// Expected: 120
// Expected: 15
const add = (a, b) => a + b;
console.log(add(3, 5));

const factorial = (n) => {
  if (n <= 1) { return 1; }
  return n * factorial(n - 1);
};
console.log(factorial(5));

const makeAdder = (x) => (y) => x + y;
const add10 = makeAdder(10);
console.log(add10(5));
