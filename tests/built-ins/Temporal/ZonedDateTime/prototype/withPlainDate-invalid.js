/*---
description: Temporal.ZonedDateTime.prototype.withPlainDate/withPlainTime throw RangeError for invalid strings
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.withPlainDate/withPlainTime invalid strings", () => {
  test("withPlainDate throws RangeError for non-date string", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    expect(() => {
      zdt.withPlainDate("not-a-date");
    }).toThrow(RangeError);
  });

  test("withPlainTime throws RangeError for non-time string", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    expect(() => {
      zdt.withPlainTime("not-a-time");
    }).toThrow(RangeError);
  });
});
