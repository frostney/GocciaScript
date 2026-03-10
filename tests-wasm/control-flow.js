// Expected: positive
// Expected: small
// Expected: yes
const x = 5;
if (x > 0) {
  console.log("positive");
} else {
  console.log("negative");
}
if (x > 100) {
  console.log("big");
} else {
  console.log("small");
}
const result = x > 0 ? "yes" : "no";
console.log(result);
