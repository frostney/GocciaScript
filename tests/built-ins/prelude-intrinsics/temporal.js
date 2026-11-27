/*---
description: Temporal built-ins (the temporal prelude) look up nothing a script can replace, beyond what ECMA-262 reads
features: [Temporal]
---*/

import { THROWS, expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

const T = (cls) => `Temporal.${cls}`;
const make = (path, ...args) => expectClean(wire.invoke(path, undefined, ...args));
const CALENDAR_GETTERS = [
  ["calendarId", []], ["era", []], ["eraYear", []], ["year", []], ["month", []], ["monthCode", []], ["day", []],
  ["dayOfWeek", []], ["dayOfYear", []], ["weekOfYear", []], ["yearOfWeek", []], ["daysInWeek", []],
  ["daysInMonth", []], ["daysInYear", []], ["monthsInYear", []], ["inLeapYear", []],
];
const TIME_GETTERS = [["hour", []], ["minute", []], ["second", []], ["millisecond", []], ["microsecond", []], ["nanosecond", []]];

test("Temporal.Duration", () => {
  const d = make(`${T("Duration")}.from`, { hours: 1, minutes: 30, seconds: 5, milliseconds: 250 });
  expect(expectClean(wire.construct(T("Duration"), 1, 2, 3, 4)).toString()).toBe("P1Y2M3W4D");
  expect(make(`${T("Duration")}.compare`, d, "PT2H")).toBe(-1);
  expect(make(`${T("Duration")}.from`, "P1Y2M3W4DT5H6M7.008009010S").nanoseconds).toBe(10);
  wire.covers(`${T("Duration")}.prototype`, d, [
    ["years", [], 0], ["months", [], 0], ["weeks", [], 0], ["days", [], 0], ["hours", [], 1], ["minutes", [], 30],
    ["seconds", [], 5], ["milliseconds", [], 250], ["microseconds", [], 0], ["nanoseconds", [], 0], ["sign", [], 1],
    ["blank", [], false],
    ["with", [{ hours: 2 }]], ["negated", []], ["abs", []], ["add", [{ minutes: 45 }]], ["subtract", ["PT10M"]],
    ["round", [{ smallestUnit: "minute", roundingMode: "halfExpand" }]], ["total", ["seconds"], 5405.25],
    ["toString", [{ fractionalSecondDigits: 2 }], "PT1H30M5.25S"], ["toJSON", [], "PT1H30M5.25S"],
    ["toLocaleString", ["en"]], ["valueOf", [], THROWS],
  ]);
  const big = make(`${T("Duration")}.from`, { seconds: 9007199254740991, nanoseconds: 999999999 });
  expect(expectClean(wire.invoke(`${T("Duration")}.prototype.toString`, big))).toBe("PT9007199254740991.999999999S");
  const rel = make(`${T("Duration")}.from`, { months: 1, days: 3 });
  expect(expectClean(wire.invoke(`${T("Duration")}.prototype.total`, rel, { unit: "days", relativeTo: "2020-01-31" }))).toBe(32);
});

test("Temporal.Instant", () => {
  const i = make(`${T("Instant")}.from`, "2020-01-15T10:20:30.123456789+05:30");
  expect(expectClean(wire.construct(T("Instant"), 0n)).epochMilliseconds).toBe(0);
  expect(make(`${T("Instant")}.fromEpochMilliseconds`, 1000).epochMilliseconds).toBe(1000);
  expect(make(`${T("Instant")}.fromEpochNanoseconds`, 10n).epochNanoseconds).toBe(10n);
  expect(make(`${T("Instant")}.compare`, i, i)).toBe(0);
  wire.covers(`${T("Instant")}.prototype`, i, [
    ["epochMilliseconds", []], ["epochNanoseconds", []], ["add", [{ hours: 1 }]], ["subtract", ["PT1M"]],
    ["until", ["2021-01-01T00:00Z", { largestUnit: "hour" }]], ["since", ["2019-01-01T00:00Z"]],
    ["round", [{ smallestUnit: "second" }]], ["equals", [i], true],
    ["toString", [{ timeZone: "Europe/Berlin", smallestUnit: "millisecond" }], "2020-01-15T05:50:30.123+01:00"],
    ["toJSON", []], ["toLocaleString", ["en"]], ["valueOf", [], THROWS], ["toZonedDateTimeISO", ["Asia/Tokyo"]],
  ]);
});

test("Temporal.PlainDate", () => {
  const d = make(`${T("PlainDate")}.from`, "2020-02-29[u-ca=iso8601]");
  expect(expectClean(wire.construct(T("PlainDate"), 2020, 1, 1, "hebrew")).monthCode).toBe("M04");
  expect(make(`${T("PlainDate")}.compare`, d, "2021-01-01")).toBe(-1);
  wire.covers(`${T("PlainDate")}.prototype`, d, [
    ...CALENDAR_GETTERS,
    ["toPlainYearMonth", []], ["toPlainMonthDay", []], ["add", [{ years: 1 }]], ["subtract", [{ months: 1, days: 5 }]],
    ["with", [{ day: 1 }]], ["withCalendar", ["japanese"]], ["until", ["2021-03-01", { largestUnit: "year" }]],
    ["since", ["2019-01-01", { largestUnit: "week" }]], ["equals", ["2020-02-29"], true],
    ["toPlainDateTime", ["10:00"]], ["toZonedDateTime", [{ timeZone: "America/New_York", plainTime: "12:00" }]],
    ["toString", [{ calendarName: "always" }], "2020-02-29[u-ca=iso8601]"], ["toJSON", [], "2020-02-29"],
    ["toLocaleString", ["en"]], ["valueOf", [], THROWS],
  ]);
  const japanese = make(`${T("PlainDate")}.from`, { era: "reiwa", eraYear: 2, monthCode: "M02", day: 29, calendar: "japanese" });
  wire.covers(`${T("PlainDate")}.prototype`, japanese, [["era", [], "reiwa"], ["eraYear", [], 2], ["with", [{ era: "reiwa", eraYear: 3, day: 28 }]]]);
  const chinese = make(`${T("PlainDate")}.from`, { year: 2020, monthCode: "M04L", day: 1, calendar: "chinese" });
  wire.covers(`${T("PlainDate")}.prototype`, chinese, [["monthCode", [], "M04L"], ["add", [{ months: 1 }]]]);
});

test("Temporal.PlainTime", () => {
  const t = make(`${T("PlainTime")}.from`, "10:20:30.000004005");
  expect(expectClean(wire.construct(T("PlainTime"), 1, 2, 3)).toString()).toBe("01:02:03");
  expect(make(`${T("PlainTime")}.compare`, t, "11:00")).toBe(-1);
  wire.covers(`${T("PlainTime")}.prototype`, t, [
    ...TIME_GETTERS, ["add", [{ hours: 15 }]], ["subtract", ["PT30M"]], ["with", [{ minute: 0 }]],
    ["until", ["23:00", { smallestUnit: "minute" }]], ["since", ["01:00"]], ["round", ["hour"]],
    ["equals", ["10:20:30.000004005"], true], ["toString", [{ smallestUnit: "microsecond" }], "10:20:30.000004"],
    ["toJSON", []], ["toLocaleString", ["en"]], ["valueOf", [], THROWS],
  ]);
});

test("Temporal.PlainDateTime", () => {
  const dt = make(`${T("PlainDateTime")}.from`, { year: 2020, month: 3, day: 8, hour: 2, minute: 30 });
  expect(expectClean(wire.construct(T("PlainDateTime"), 2020, 1, 1, 12)).hour).toBe(12);
  expect(make(`${T("PlainDateTime")}.compare`, dt, dt)).toBe(0);
  wire.covers(`${T("PlainDateTime")}.prototype`, dt, [
    ...CALENDAR_GETTERS, ...TIME_GETTERS,
    ["with", [{ hour: 3 }]], ["withPlainTime", ["04:00"]], ["withCalendar", ["gregory"]], ["add", [{ days: 1 }]],
    ["subtract", [{ hours: 30 }]], ["until", ["2021-01-01T00:00", { largestUnit: "month" }]], ["since", ["2020-01-01"]],
    ["round", [{ smallestUnit: "hour", roundingMode: "ceil" }]], ["equals", [dt], true],
    ["toString", [{ smallestUnit: "minute" }], "2020-03-08T02:30"], ["toJSON", []], ["toLocaleString", ["en"]],
    ["valueOf", [], THROWS], ["toZonedDateTime", ["America/New_York", { disambiguation: "later" }]],
    ["toPlainDate", []], ["toPlainTime", []],
  ]);
});

test("Temporal.PlainYearMonth", () => {
  const ym = make(`${T("PlainYearMonth")}.from`, "2020-02");
  expect(expectClean(wire.construct(T("PlainYearMonth"), 2020, 5)).month).toBe(5);
  expect(make(`${T("PlainYearMonth")}.compare`, ym, "2020-03")).toBe(-1);
  wire.covers(`${T("PlainYearMonth")}.prototype`, ym, [
    ["calendarId", []], ["era", []], ["eraYear", []], ["year", []], ["month", []], ["monthCode", []],
    ["daysInYear", []], ["daysInMonth", [], 29], ["monthsInYear", []], ["inLeapYear", [], true],
    ["with", [{ month: 12 }]], ["add", [{ months: 13 }]], ["subtract", [{ years: 1 }]],
    ["until", ["2021-06", { largestUnit: "month" }]], ["since", ["2019-01"]], ["equals", ["2020-02"], true],
    ["toString", [], "2020-02"], ["toJSON", []], ["toLocaleString", ["en", { calendar: "iso8601" }]],
    ["valueOf", [], THROWS], ["toPlainDate", [{ day: 10 }]],
  ]);
});

test("Temporal.PlainMonthDay", () => {
  const md = make(`${T("PlainMonthDay")}.from`, "--02-29");
  expect(expectClean(wire.construct(T("PlainMonthDay"), 12, 25)).day).toBe(25);
  expect(typeof make(`${T("PlainMonthDay")}.compare`, md, "02-28")).toBe("number");
  wire.covers(`${T("PlainMonthDay")}.prototype`, md, [
    ["calendarId", []], ["monthCode", [], "M02"], ["day", [], 29], ["with", [{ day: 1 }]],
    ["equals", ["02-29"], true], ["toString", [], "02-29"], ["toJSON", []],
    ["toLocaleString", ["en", { calendar: "iso8601" }]], ["valueOf", [], THROWS], ["toPlainDate", [{ year: 2021 }]],
  ]);
});

test("Temporal.ZonedDateTime", () => {
  const z = make(`${T("ZonedDateTime")}.from`, "2020-03-08T01:30-05:00[America/New_York]");
  expect(expectClean(wire.construct(T("ZonedDateTime"), 0n, "UTC")).year).toBe(1970);
  expect(make(`${T("ZonedDateTime")}.compare`, z, z)).toBe(0);
  const fixed = make(`${T("ZonedDateTime")}.from`, "2020-01-01T00:00+05:30[+05:30]");
  expect(fixed.offset).toBe("+05:30");
  wire.covers(`${T("ZonedDateTime")}.prototype`, z, [
    ...CALENDAR_GETTERS, ...TIME_GETTERS,
    ["timeZoneId", [], "America/New_York"], ["epochMilliseconds", []], ["epochNanoseconds", []],
    ["hoursInDay", [], 23], ["offsetNanoseconds", []], ["offset", [], "-05:00"],
    ["with", [{ hour: 2, minute: 30 }, { disambiguation: "compatible", offset: "prefer" }]],
    ["withPlainTime", ["12:00"]], ["withTimeZone", ["Europe/London"]], ["withCalendar", ["gregory"]],
    ["add", [{ hours: 1 }]], ["subtract", [{ days: 1 }]], ["until", ["2020-03-09T00:00-04:00[America/New_York]", { largestUnit: "hour" }]],
    ["since", ["2020-03-01T00:00-05:00[America/New_York]", { largestUnit: "day" }]],
    ["round", [{ smallestUnit: "day" }]], ["equals", [z], true],
    ["toString", [{ timeZoneName: "critical", offset: "auto" }], "2020-03-08T01:30:00-05:00[!America/New_York]"],
    ["toJSON", []], ["toLocaleString", ["en"]], ["valueOf", [], THROWS], ["startOfDay", []],
    ["getTimeZoneTransition", ["next"]], ["toInstant", []], ["toPlainDate", []], ["toPlainTime", []],
    ["toPlainDateTime", []],
  ]);
});

test("Temporal.Now", () => {
  wire.covers("Temporal.Now", Temporal.Now, [
    ["instant", []], ["timeZoneId", []], ["zonedDateTimeISO", ["Europe/Paris"]], ["plainDateTimeISO", ["UTC"]],
    ["plainDateISO", []], ["plainTimeISO", ["+01:00"]],
  ]);
});

test("every Temporal method, getter and constructor has a case", () => {
  const owners = ["Temporal", "Temporal.Now"];
  for (const C of ["Duration", "Instant", "PlainDate", "PlainDateTime", "PlainMonthDay", "PlainTime", "PlainYearMonth", "ZonedDateTime"]) {
    owners.push(T(C), `${T(C)}.prototype`);
  }
  expect(owners.flatMap((o) => wire.uncovered(o))).toEqual([]);
});
