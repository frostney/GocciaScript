/*---
description: >
  Temporal.ZonedDateTime.prototype.getTimeZoneTransition finds transitions
  that a named zone's daylight-saving rule produces after its last listed
  transition, within the Temporal instant range.
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

const transition = (instant, timeZone, direction) => {
  const result = Temporal.Instant.from(instant).toZonedDateTimeISO(timeZone)
    .getTimeZoneTransition(direction);
  return result === null ? null : result.toInstant().toString();
};

const MAX_EPOCH_NANOSECONDS = 8640000000000000000000n;

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.getTimeZoneTransition", () => {
  test("next finds a rule transition after 2037", () => {
    expect(transition("2040-01-01T00:00:00Z", "America/Los_Angeles", "next"))
      .toBe("2040-03-11T10:00:00Z");
    expect(transition("2040-01-01T00:00:00Z", "Australia/Sydney", "next"))
      .toBe("2040-03-31T16:00:00Z");
    expect(transition("2040-01-01T00:00:00Z", "Europe/Dublin", "next"))
      .toBe("2040-03-25T01:00:00Z");
  });

  test("next steps from one rule transition to the following one", () => {
    expect(transition("2040-03-11T10:00:00Z", "America/Los_Angeles", "next"))
      .toBe("2040-11-04T09:00:00Z");
  });

  test("next crosses from the last listed transition into the rule", () => {
    expect(transition("2037-11-01T09:00:00Z", "America/Los_Angeles", "next"))
      .toBe("2038-03-14T10:00:00Z");
  });

  test("previous finds a rule transition after 2037", () => {
    expect(transition("2040-07-01T00:00:00Z", "America/Los_Angeles", "previous"))
      .toBe("2040-03-11T10:00:00Z");
    expect(transition("2040-03-11T10:00:00.000000001Z", "America/Los_Angeles", "previous"))
      .toBe("2040-03-11T10:00:00Z");
    expect(transition("2040-03-11T10:00:00Z", "America/Los_Angeles", "previous"))
      .toBe("2039-11-06T09:00:00Z");
  });

  test("a zone that stopped observing daylight saving time has no later transition", () => {
    expect(transition("2040-01-01T00:00:00Z", "Asia/Tokyo", "next")).toBe(null);
    expect(transition("2040-01-01T00:00:00Z", "America/Sao_Paulo", "next")).toBe(null);
  });

  // The previous transition from the largest instant is checked in
  // scripts/test-cli-apps.ts: Windows reads ICU's transitions only up to 2200.
  test("no transition is reported beyond the largest instant", () => {
    const max = new Temporal.ZonedDateTime(MAX_EPOCH_NANOSECONDS, "America/Los_Angeles");
    expect(max.getTimeZoneTransition("next")).toBe(null);
  });
});
