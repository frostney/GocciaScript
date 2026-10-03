/*---
description: The regular expression step budget grows with the subject length
features: [RegExp, Goccia]
---*/

// The step budget is 100 steps per code unit, at least 10,000,000. From
// 21,474,837 code units the product no longer fits in 32 bits: a range-checked
// build stopped with a fatal range check error, and an unchecked build used a
// wrapped budget of 10,000,000 steps. Goccia.RegExp.VM.Test pins the budget
// arithmetic on every target.
//
// This test needs a 21.5-million-code-unit subject and its decoded form, about
// 210 MB at peak, which a 32-bit process running parallel test workers cannot
// spare, so it runs on 64-bit targets only.
const is64Bit = typeof Goccia !== "undefined" &&
  ["x86_64", "aarch64", "powerpc64"].includes(Goccia.build.arch);

test.runIf(is64Bit)("a match needing more than ten million steps succeeds on a subject of 21,474,837 code units or more", () => {
  const block = "a".repeat(1000);
  const subject = block.repeat(13000) + "b" + block.repeat(8475);
  // About 54 steps per 50 code units, so reaching the "b" at index 13,000,000
  // takes about 14 million steps.
  const pattern = new RegExp("(?:" + "a".repeat(50) + ")*b", "y");

  expect(subject.length).toBe(21475001);
  expect(pattern.test(subject)).toBe(true);
  expect(pattern.lastIndex).toBe(13000001);
  // The VM keeps the decoded form of the last subject; a match on a short
  // subject releases it.
  expect(/a/.test("a")).toBe(true);
});
