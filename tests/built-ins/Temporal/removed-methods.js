const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal removed methods", () => {
  test("does not expose methods removed from the Temporal proposal", () => {
    expect("toPlainMonthDay" in Temporal.PlainDateTime.prototype).toBe(false);
    expect("toPlainYearMonth" in Temporal.PlainDateTime.prototype).toBe(false);
    expect("withPlainDate" in Temporal.PlainDateTime.prototype).toBe(false);
    expect("toPlainMonthDay" in Temporal.ZonedDateTime.prototype).toBe(false);
    expect("toPlainYearMonth" in Temporal.ZonedDateTime.prototype).toBe(false);
    expect("withPlainDate" in Temporal.ZonedDateTime.prototype).toBe(false);
  });
});
