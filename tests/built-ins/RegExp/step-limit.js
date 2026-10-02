/*---
description: The regular expression step budget grows with the subject length
features: [RegExp]
---*/

// The step budget is 100 steps per code unit, at least 10,000,000. From
// 21,474,837 code units the product no longer fits in 32 bits: a range-checked
// build stopped with a fatal range check error, and an unchecked build used a
// wrapped budget of 10,000,000 steps.
test("a match needing more than ten million steps succeeds on a subject of 21,474,837 code units or more", () => {
  const block = "a".repeat(1000);
  const subject = block.repeat(10100) + "b" + block.repeat(11375);
  // Each iteration takes 52 steps for 50 code units, so reaching the "b" at
  // index 10,100,000 takes about 10.5 million steps.
  const pattern = new RegExp("(?:" + "a".repeat(50) + ")*b", "y");

  expect(subject.length).toBe(21475001);
  expect(pattern.test(subject)).toBe(true);
  expect(pattern.lastIndex).toBe(10100001);
});
