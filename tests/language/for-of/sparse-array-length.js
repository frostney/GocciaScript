/*---
description: for...of over an array whose length is past its stored elements
features: [for-of, Array]
---*/

// An array iterator reads the array's length at every step
// (ES2026 §23.1.5.2.1 %ArrayIteratorPrototype%.next), so elements past the
// last stored one are visited as undefined.

// A length past the dense-extension gap leaves the tail sparse instead of
// dense holes, and shrinking the length from there keeps it sparse. A
// GC-stress build collects every 8 instructions, so each step costs a pass
// over the live heap and 2,000,000 steps per loop take about an hour there;
// a stress run uses a shorter sparse length. An element stored far past the
// others is dense holes at that length, so the sparse case of that test runs
// only in other builds.
const sparseLength = 2000000;
const farLength = (typeof Goccia !== "undefined" && Goccia.build.gcStress) ? 2000 : sparseLength;

test("for...of visits every index up to a length set far past the elements", () => {
  const array = [1, 2];
  array.length = sparseLength;
  array.length = farLength;
  let count = 0;
  let lastValue = null;
  for (const value of array) {
    count += 1;
    lastValue = value;
  }
  expect(count).toBe(farLength);
  expect(lastValue).toBe(undefined);
});

test("for...of reads an element stored far past the others", () => {
  const array = [1];
  array[farLength - 1] = "last";
  let count = 0;
  let lastValue = null;
  for (const value of array) {
    count += 1;
    lastValue = value;
  }
  expect(count).toBe(farLength);
  expect(lastValue).toBe("last");
});

test("for...of over a type-annotated array follows its length too", () => {
  const annotated: number[] = [1, 2];
  annotated.length = sparseLength;
  annotated.length = farLength;
  let count = 0;
  for (const value of annotated) {
    count += 1;
  }
  expect(count).toBe(farLength);
});
