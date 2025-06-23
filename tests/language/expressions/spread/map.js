/*---
description: Spread syntax for Map
features: [map-spread]
---*/

test.skip("spread syntax for Map", () => {
  const map = new Map([
    ["a", 1],
    ["b", 2],
  ]);
  const newMap = [...map, ["c", 3], ["d", 4]];
  expect(newMap).toEqual([
    ["a", 1],
    ["b", 2],
    ["c", 3],
    ["d", 4],
  ]);
});

test.skip("spread syntax to copy map content", () => {
  const map = new Map([
    ["a", 1],
    ["b", 2],
  ]);
  const entries = [...map];
  expect(entries).toEqual([
    ["a", 1],
    ["b", 2],
  ]);
});
