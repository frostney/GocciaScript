/*---
description: Temporal.ZonedDateTime.from
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";
const hasCalendarICU = isTemporal && (() => {
  try {
    return Temporal.PlainDate.from({
      year: 1445,
      month: 12,
      day: 25,
      calendar: "islamicc"
    }).calendarId === "islamic-civil";
  } catch (_) {
    return false;
  }
})();

describe.runIf(isTemporal)("Temporal.ZonedDateTime.from", () => {
  test("from ISO string with timezone", () => {
    const zdt = Temporal.ZonedDateTime.from("2024-03-15T13:45:30+00:00[UTC]");
    expect(zdt.year).toBe(2024);
    expect(zdt.month).toBe(3);
    expect(zdt.day).toBe(15);
    expect(zdt.hour).toBe(13);
    expect(zdt.timeZoneId).toBe("UTC");
  });

  test("from() accepts +000000 as year zero", () => {
    const zdt = Temporal.ZonedDateTime.from("+000000-03-31T00:45+00:00[UTC]");
    expect(zdt.year).toBe(0);
    expect(zdt.month).toBe(3);
    expect(zdt.day).toBe(31);
  });

  test("from property bag with ISO time zone", () => {
    const zdt = Temporal.ZonedDateTime.from({
      year: 2024,
      monthCode: "M03",
      day: 15,
      hour: 13,
      minute: 45,
      second: 30,
      timeZone: "UTC"
    });
    expect(zdt.year).toBe(2024);
    expect(zdt.month).toBe(3);
    expect(zdt.day).toBe(15);
    expect(zdt.hour).toBe(13);
    expect(zdt.minute).toBe(45);
    expect(zdt.second).toBe(30);
    expect(zdt.timeZoneId).toBe("UTC");
  });

  test("from property bag preserves sub-minute named-zone offsets", () => {
    const zdt = Temporal.ZonedDateTime.from({
      year: 1971,
      month: 1,
      day: 1,
      hour: 12,
      timeZone: "Africa/Monrovia"
    });
    expect(zdt.offset).toBe("-00:44:30");
  });

  test("from property bag converts supported non-ISO calendar dates", () => {
    if (!hasCalendarICU) {
      expect(() => Temporal.ZonedDateTime.from({
        year: 1445,
        month: 12,
        day: 25,
        calendar: "islamicc",
        timeZone: "UTC"
      })).toThrow(RangeError);
      return;
    }
    const zdt = Temporal.ZonedDateTime.from({
      year: 1445,
      month: 12,
      day: 25,
      calendar: "islamicc",
      timeZone: "UTC"
    });
    const iso = Temporal.ZonedDateTime.from("2024-07-02T00:00:00+00:00[UTC]");
    expect(zdt.epochNanoseconds).toBe(iso.epochNanoseconds);
    expect(zdt.calendarId).toBe("islamic-civil");
  });

  test("disambiguation resolves a daylight-saving gap after 2037", () => {
    const from = (disambiguation) => Temporal.ZonedDateTime.from(
      "2040-03-11T02:30[America/Los_Angeles]", { disambiguation }).toString();
    expect(from("compatible")).toBe("2040-03-11T03:30:00-07:00[America/Los_Angeles]");
    expect(from("earlier")).toBe("2040-03-11T01:30:00-08:00[America/Los_Angeles]");
    expect(from("later")).toBe("2040-03-11T03:30:00-07:00[America/Los_Angeles]");
    expect(() => from("reject")).toThrow(RangeError);
  });

  test("disambiguation resolves a daylight-saving fold after 2037", () => {
    const from = (disambiguation) => Temporal.ZonedDateTime.from(
      "2040-11-04T01:30[America/Los_Angeles]", { disambiguation }).toString();
    expect(from("compatible")).toBe("2040-11-04T01:30:00-07:00[America/Los_Angeles]");
    expect(from("earlier")).toBe("2040-11-04T01:30:00-07:00[America/Los_Angeles]");
    expect(from("later")).toBe("2040-11-04T01:30:00-08:00[America/Los_Angeles]");
    expect(() => from("reject")).toThrow(RangeError);
  });

  test("a wall-clock time in a negative daylight-saving gap after 2037 moves forward", () => {
    expect(Temporal.ZonedDateTime.from("2040-03-25T01:30[Europe/Dublin]").toString())
      .toBe("2040-03-25T02:30:00+01:00[Europe/Dublin]");
  });
});
