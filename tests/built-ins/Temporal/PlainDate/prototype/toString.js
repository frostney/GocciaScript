/*---
description: Temporal.PlainDate.prototype.toString
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.toString", () => {
  test("toString()", () => {
    expect(new Temporal.PlainDate(2024, 3, 15).toString()).toBe("2024-03-15");
    expect(new Temporal.PlainDate(2024, 1, 1).toString()).toBe("2024-01-01");
  });

  test("toString includes non-ISO calendar annotation by default", () => {
    expect(new Temporal.PlainDate(2024, 3, 15, "gregory").toString()).toBe("2024-03-15[u-ca=gregory]");
  });

  test("calendarName controls calendar annotation", () => {
    const d = new Temporal.PlainDate(2024, 3, 15, "gregory");
    expect(d.toString({ calendarName: "never" })).toBe("2024-03-15");
    expect(d.toString({ calendarName: "critical" })).toBe("2024-03-15[!u-ca=gregory]");
  });
});
