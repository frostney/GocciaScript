/*---
description: Goccia.semver.Comparator constructs comparator objects
features: [Goccia.semver.Comparator]
---*/

test("new Goccia.semver.Comparator builds comparator instances", () => {
  const comparator = new Goccia.semver.Comparator(">=1.2.3");
  expect(comparator instanceof Goccia.semver.Comparator).toBe(true);
  expect(comparator.operator).toBe(">=");
  expect(comparator.value).toBe(">=1.2.3");
  expect(comparator.test("1.2.3")).toBe(true);
  expect(comparator.test("1.2.2")).toBe(false);
});
