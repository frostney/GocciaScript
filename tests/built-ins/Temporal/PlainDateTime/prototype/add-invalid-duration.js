/*---
description: Temporal.PlainDateTime.prototype.add/subtract/withPlainTime throw RangeError for invalid strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDateTime.prototype.add/subtract/withPlainTime invalid strings", () => {
  test("add throws RangeError for non-duration string", () => {
    const dt = new Temporal.PlainDateTime(2024, 1, 1);
    expect(() => {
      dt.add("not-a-duration");
    }).toThrow(RangeError);
  });

  test("subtract throws RangeError for non-duration string", () => {
    const dt = new Temporal.PlainDateTime(2024, 1, 1);
    expect(() => {
      dt.subtract("not-a-duration");
    }).toThrow(RangeError);
  });

  test("withPlainTime throws RangeError for non-time string", () => {
    const dt = new Temporal.PlainDateTime(2024, 1, 1);
    expect(() => {
      dt.withPlainTime("not-a-time");
    }).toThrow(RangeError);
  });
});
