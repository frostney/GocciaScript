/*---
description: for...of over an array whose length is past its stored elements
features: [for-of, Array]
---*/

// An array iterator reads the array's length at every step
// (ES2026 §23.1.5.2.1 %ArrayIteratorPrototype%.next), so elements past the
// last stored one are visited as undefined.

// 2,000,000 is past the dense-extension gap, so the tail stays sparse instead
// of dense holes. A GC-stress build collects every 8 instructions, so each
// step costs a pass over the live heap and the three loops take about an hour
// there; a stress run uses a length whose tail is dense holes.
const farLength = (typeof Goccia !== "undefined" && Goccia.build.gcStress) ? 2000 : 2000000;

test("for...of visits every index up to a length set far past the elements", () => {
  const array = [1, 2];
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
  annotated.length = farLength;
  let count = 0;
  for (const value of annotated) {
    count += 1;
  }
  expect(count).toBe(farLength);
});
