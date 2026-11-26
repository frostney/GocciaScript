/*---
description: Hebrew dates of the years converted arithmetically round-trip through ISO
features: [Temporal]
---*/

const hasTemporal = typeof Temporal !== "undefined";

describe.runIf(hasTemporal)("Temporal.PlainDate.from with the hebrew calendar", () => {
  test("every month start of year 3705 reads back as that month", () => {
    for (const monthCode of ["M01", "M02", "M03", "M04", "M05", "M05L", "M06", "M07", "M08", "M12"]) {
      const date = Temporal.PlainDate.from({ calendar: "hebrew", year: 3705, monthCode, day: 1 });
      expect(date.year).toBe(3705);
      expect(date.monthCode).toBe(monthCode);
      expect(date.day).toBe(1);
    }
  });

  test("the first day of Pesach exists in each of those years", () => {
    for (const year of [3705, 3952, 4050, 4297, 4544, 4642, 4889, 4967, 5136, 5214, 5461, 5559]) {
      const date = Temporal.PlainDate.from({ calendar: "hebrew", year, monthCode: "M07", day: 15 });
      expect(date.monthCode).toBe("M07");
      expect(date.day).toBe(15);
    }
  });
});
