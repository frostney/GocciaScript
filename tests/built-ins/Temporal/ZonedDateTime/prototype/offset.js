/*---
description: >
  Temporal.ZonedDateTime.prototype.offset follows a named zone's current
  daylight-saving rule after its last listed transition (2037 in zoneinfo
  data), as V8 and ICU do.
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

const offsetAt = (instant, timeZone) =>
  Temporal.Instant.from(instant).toZonedDateTimeISO(timeZone).offset;

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.offset", () => {
  test("a northern-hemisphere zone keeps daylight saving time after 2037", () => {
    expect(offsetAt("2038-01-15T12:00:00Z", "America/Los_Angeles")).toBe("-08:00");
    expect(offsetAt("2038-07-01T12:00:00Z", "America/Los_Angeles")).toBe("-07:00");
    expect(offsetAt("2049-05-01T07:00:00Z", "America/Los_Angeles")).toBe("-07:00");
  });

  test("a southern-hemisphere zone has daylight saving time in its summer after 2037", () => {
    expect(offsetAt("2038-01-15T12:00:00Z", "Australia/Sydney")).toBe("+11:00");
    expect(offsetAt("2038-07-01T12:00:00Z", "Australia/Sydney")).toBe("+10:00");
    expect(offsetAt("2038-01-15T12:00:00Z", "America/Santiago")).toBe("-03:00");
    expect(offsetAt("2038-07-01T12:00:00Z", "America/Santiago")).toBe("-04:00");
  });

  test("a zone with negative daylight saving time keeps its summer offset after 2037", () => {
    expect(offsetAt("2038-01-15T12:00:00Z", "Europe/Dublin")).toBe("+00:00");
    expect(offsetAt("2038-07-01T12:00:00Z", "Europe/Dublin")).toBe("+01:00");
  });

  test("a half-hour daylight saving shift continues after 2037", () => {
    expect(offsetAt("2040-01-15T00:00:00Z", "Australia/Lord_Howe")).toBe("+11:00");
    expect(offsetAt("2040-07-15T00:00:00Z", "Australia/Lord_Howe")).toBe("+10:30");
  });

  test("a zone without daylight saving time keeps one offset", () => {
    expect(offsetAt("2100-07-01T12:00:00Z", "Asia/Tokyo")).toBe("+09:00");
    expect(offsetAt("2100-07-01T12:00:00Z", "America/Sao_Paulo")).toBe("-03:00");
  });

  // Windows reads ICU's transitions only up to 2200, so this suite stays
  // before then; scripts/test-cli-apps.ts checks the largest instant
  // against a TZif file on every platform.
  test("the rule holds in later centuries", () => {
    expect(offsetAt("2199-01-15T12:00:00Z", "America/Los_Angeles")).toBe("-08:00");
    expect(offsetAt("2199-07-01T12:00:00Z", "America/Los_Angeles")).toBe("-07:00");
    expect(offsetAt("2199-01-15T12:00:00Z", "Australia/Sydney")).toBe("+11:00");
    expect(offsetAt("2199-07-01T12:00:00Z", "Australia/Sydney")).toBe("+10:00");
  });
});
