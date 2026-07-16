/*---
description: Date.UTC
features: [Date]
---*/

test("returns an epoch millisecond timestamp", () => {
  expect(Date.UTC(2024, 5, 15, 11, 30, 45, 123)).toBe(1718451045123);
});

test("normalizes overflowing date and time fields", () => {
  expect(Date.UTC(2020, 12, 1)).toBe(Date.UTC(2021, 0, 1));
  expect(Date.UTC(2020, 0, 32)).toBe(Date.UTC(2020, 1, 1));
  expect(Date.UTC(2020, -1, 1)).toBe(Date.UTC(2019, 11, 1));
  expect(Date.UTC(2020, 0, 1, 24, 60, 60, 1000)).toBe(Date.UTC(2020, 0, 2, 1, 1, 1, 0));
});

test("applies TimeClip after normalization", () => {
  expect(Number.isNaN(Date.UTC(1970, 0, -99999999, 0, -60, 0, -1))).toBe(true);
});

test("distinguishes omitted and explicit undefined arguments", () => {
  expect(Number.isNaN(Date.UTC())).toBe(true);
  expect(Date.UTC(2020)).toBe(Date.UTC(2020, 0, 1));
  expect(Number.isNaN(Date.UTC(2020, undefined))).toBe(true);
});
