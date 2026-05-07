/*---
description: -000000 is not a valid extended year representation
---*/

const isTemporal = typeof Temporal !== "undefined";

describe("Date year zero", () => {
  test("new Date('-000000-03-31T00:45Z') is invalid", () => {
    expect(isNaN(new Date("-000000-03-31T00:45Z").getTime())).toBe(true);
  });

  test("new Date('+000000-03-31T00:45Z') is valid", () => {
    expect(isNaN(new Date("+000000-03-31T00:45Z").getTime())).toBe(false);
  });
});

describe.runIf(isTemporal)("Temporal year zero", () => {
  test("Temporal.Instant.from rejects -000000", () => {
    expect(() => {
      Temporal.Instant.from("-000000-03-31T00:45Z");
    }).toThrow(RangeError);
  });

  test("Temporal.PlainDate.from rejects -000000", () => {
    expect(() => {
      Temporal.PlainDate.from("-000000-03-31");
    }).toThrow(RangeError);
  });

  test("Temporal.PlainDateTime.from rejects -000000", () => {
    expect(() => {
      Temporal.PlainDateTime.from("-000000-03-31T00:45");
    }).toThrow(RangeError);
  });

  test("Temporal.ZonedDateTime.from rejects -000000", () => {
    expect(() => {
      Temporal.ZonedDateTime.from("-000000-03-31T00:45[UTC]");
    }).toThrow(RangeError);
  });

  test("Temporal.PlainYearMonth.from rejects -000000", () => {
    expect(() => {
      Temporal.PlainYearMonth.from("-000000-03");
    }).toThrow(RangeError);
  });
});
