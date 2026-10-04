/*---
description: Greedy single-character loops backtrack through every shorter count without one backtrack entry per iteration
features: [RegExp, Goccia]
---*/

// A loop whose body matches one character (a*, .*, [a-z]*) used to keep one
// backtrack entry per iteration, so a match over more than 10,000,000
// characters hit the backtrack-stack cap. One entry now stands for every
// shorter count. These subjects are longer than 32 code units, which the VM
// requires before it takes this path.

test("a greedy loop backtracks to shorter counts when the rest of the pattern needs them", () => {
  const subject = "x" + "a".repeat(40) + "ab";
  expect(/xa*ab/.exec(subject)[0]).toBe(subject);
  expect(/x(a*)aab/.exec(subject)[1]).toBe("a".repeat(39));
  expect(/x(a*)a{41}b/.exec(subject)[1]).toBe("");
  expect(/xa*a{42}b/.test(subject)).toBe(false);
});

test("a greedy dot loop stops at a line terminator and backtracks from there", () => {
  const subject = "q" + "z".repeat(40) + "\nzq";
  expect(/q(.*)z/.exec(subject)[1]).toBe("z".repeat(39));
  expect(/q(.*)q/.test(subject)).toBe(false);
  expect(/q(.*)q/s.exec(subject)[1]).toBe("z".repeat(40) + "\nz");
});

test("a greedy class loop backtracks over surrogate pairs by code point in unicode mode", () => {
  const emoji = "\u{1F600}";
  const subject = emoji.repeat(40) + "x";
  expect(/^.*(.)x/u.exec(subject)[1]).toBe(emoji);
  expect(/^.*(.)x/.exec(subject)[1]).toBe("\ude00");
  expect(/^[^x]*(.)(.)x/u.exec(subject).slice(1)).toEqual([emoji, emoji]);
});

test("each shorter count is tried after the longer ones fail", () => {
  const subject = "a".repeat(40) + "b".repeat(5);
  const match = /^(a*)(a{3}b+)$/.exec(subject);
  expect(match[1].length).toBe(37);
  expect(match[2]).toBe("aaabbbbb");
  expect(/^(a*)?a{40}b{5}$/.exec(subject)[1]).toBeUndefined();
});

test("a loop at the end of the pattern is rescanned for every count of an earlier loop without reaching the step limit", () => {
  // [ab]* gives back one character at a time, and a* rescans the rest each
  // time: 12.5 million characters for 5,000, more than the step limit.
  expect(/^[ab]*a*$/.test("a".repeat(5000) + "!")).toBe(false);
});

test("a greedy loop prunes the same failed states on a long run as on a short one", () => {
  // Under 32 code units the loop iterates one character at a time; from 32
  // on it keeps one entry for the whole run. Both find the same match.
  const short = /(.*)[ab]*\1{3}/.exec("bccc");
  const long = /(.*)[ab]*\1{3}/.exec("b" + "c".repeat(48));
  expect(long.index).toBe(short.index);
  expect(long[1]).toBe(short[1]);
  expect(long[0]).toBe(short[0]);
});

test("the step limit reports its own message", () => {
  let message = "";
  try {
    /^(a+)+$/.test("a".repeat(30) + "b");
  } catch (e) {
    message = e.message;
  }
  expect(message).toBe("Maximum regular expression step count exceeded");
});

// The subject and its decoded form take about 60 MB, more than a 32-bit
// process running parallel test workers should spend, so this runs on 64-bit
// targets only.
const is64Bit = typeof Goccia !== "undefined" &&
  ["x86_64", "aarch64", "powerpc64"].includes(Goccia.build.arch);

test.runIf(is64Bit)("a greedy loop over more than 10,000,000 characters matches", () => {
  const subject = "a".repeat(10000001) + "b";
  expect(/a*b/.test(subject)).toBe(true);
  expect(/.*b/.test(subject)).toBe(true);
  expect(/[a-z]*b/.exec(subject)[0].length).toBe(10000002);
});
