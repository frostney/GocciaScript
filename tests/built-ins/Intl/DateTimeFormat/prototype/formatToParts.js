/*---
description: Intl.DateTimeFormat.prototype.formatToParts
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

const partsString = (parts) => {
  return parts.map((part) => part.value).join("");
};

const expectCurrentDateParts = (readActual, formatAt) => {
  const before = Date.now();
  const actual = partsString(readActual());
  const after = Date.now();
  const beforeFormatted = partsString(formatAt(before));
  const afterFormatted = partsString(formatAt(after));

  expect(actual === beforeFormatted || actual === afterFormatted).toBe(true);
};

describe.runIf(isIntl)("Intl.DateTimeFormat.prototype.formatToParts", () => {
  test("formatToParts uses the current date for omitted and undefined values", () => {
    const dtf = new Intl.DateTimeFormat("en-US", { timeZone: "UTC" });
    expectCurrentDateParts(() => dtf.formatToParts(), (value) => dtf.formatToParts(value));
    expectCurrentDateParts(() => dtf.formatToParts(undefined), (value) => dtf.formatToParts(value));
  });

  test("formatToParts rejects invalid numeric dates", () => {
    const dtf = new Intl.DateTimeFormat("en-US");
    expect(() => dtf.formatToParts(NaN)).toThrow(RangeError);
  });
});
