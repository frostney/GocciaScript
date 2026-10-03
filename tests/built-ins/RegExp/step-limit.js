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
  const subject = block.repeat(13000) + "b" + block.repeat(8475);
  // About 54 steps per 50 code units, so reaching the "b" at index 13,000,000
  // takes about 14 million steps.
  const pattern = new RegExp("(?:" + "a".repeat(50) + ")*b", "y");

  expect(subject.length).toBe(21475001);
  expect(pattern.test(subject)).toBe(true);
  expect(pattern.lastIndex).toBe(13000001);
});
