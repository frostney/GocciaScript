/*---
description: for...of over an array whose length is past its stored elements
features: [for-of, Array]
---*/

// An array iterator reads the array's length at every step
// (ES2026 §23.1.5.2.1 %ArrayIteratorPrototype%.next), so elements past the
// last stored one are visited as undefined.

// A length past the dense-extension gap leaves the tail sparse instead of
// dense holes, and shrinking the length from there keeps it sparse, so a
// short loop still runs past the stored elements. An element stored far past
// the others is sparse only at that distance, so "for...of reads an element
// stored far past the others" loops 2,000,000 steps. A GC-stress build
// collects every 8 instructions, which makes that loop take about 20 minutes
// there; a stress run stores the element closer, where it is dense holes, so
// the sparse case of that test runs only in other builds.
const sparseLength = 2000000;
const shortLength = 2000;
const farLength = (typeof Goccia !== "undefined" && Goccia.build.gcStress) ? shortLength : sparseLength;

test("for...of visits every index up to a length set far past the elements", () => {
  const array = [1, 2];
  array.length = sparseLength;
  array.length = shortLength;
  let count = 0;
  let lastValue = null;
  for (const value of array) {
    count += 1;
    lastValue = value;
  }
  expect(count).toBe(shortLength);
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
  annotated.length = shortLength;
  let count = 0;
  for (const value of annotated) {
    count += 1;
  }
  expect(count).toBe(shortLength);
});
