/*---
description: Temporal.Instant construction, properties, and methods
features: [Temporal]
---*/

describe("Temporal.Instant", () => {

  test("fromEpochMilliseconds()", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    expect(instant.epochMilliseconds).toBe(0);
  });

  test("fromEpochMilliseconds with positive value", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(1000);
    expect(instant.epochMilliseconds).toBe(1000);
  });

  test("fromEpochMilliseconds with negative value", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(-86400000);
    expect(instant.epochMilliseconds).toBe(-86400000);
  });

  test("toString() for epoch", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    expect(instant.toString()).toBe("1970-01-01T00:00:00Z");
  });

  test("toString() for specific time", () => {
    // 2024-01-15T12:30:45.000Z = epoch ms
    const ms = 1705321845000;
    const instant = Temporal.Instant.fromEpochMilliseconds(ms);
    expect(instant.toString()).toBe("2024-01-15T12:30:45Z");
  });

  test("toJSON()", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    expect(instant.toJSON()).toBe(instant.toString());
  });

  test("equals()", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(1000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(1000);
    const i3 = Temporal.Instant.fromEpochMilliseconds(2000);
    expect(i1.equals(i2)).toBe(true);
    expect(i1.equals(i3)).toBe(false);
  });

  test("add() with time duration", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(0);
    const result = instant.add(new Temporal.Duration(0, 0, 0, 0, 1));
    expect(result.epochMilliseconds).toBe(3600000);
  });

  test("subtract() with time duration", () => {
    const instant = Temporal.Instant.fromEpochMilliseconds(3600000);
    const result = instant.subtract(new Temporal.Duration(0, 0, 0, 0, 1));
    expect(result.epochMilliseconds).toBe(0);
  });

  test("until()", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(0);
    const i2 = Temporal.Instant.fromEpochMilliseconds(3600000);
    const dur = i1.until(i2);
    expect(dur.hours).toBe(1);
    expect(dur.minutes).toBe(0);
  });

  test("since()", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(3600000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(0);
    const dur = i1.since(i2);
    expect(dur.hours).toBe(1);
  });

  test("from() with Instant", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(5000);
    const i2 = Temporal.Instant.from(i1);
    expect(i2.epochMilliseconds).toBe(5000);
  });

  test("compare()", () => {
    const i1 = Temporal.Instant.fromEpochMilliseconds(1000);
    const i2 = Temporal.Instant.fromEpochMilliseconds(2000);
    expect(Temporal.Instant.compare(i1, i2)).toBe(-1);
    expect(Temporal.Instant.compare(i2, i1)).toBe(1);
    expect(Temporal.Instant.compare(i1, i1)).toBe(0);
  });

  test("round()", () => {
    // 1500ms = 1.5 seconds, rounds to 2 seconds
    const instant = Temporal.Instant.fromEpochMilliseconds(1500);
    const rounded = instant.round("second");
    expect(rounded.epochMilliseconds).toBe(2000);
  });

});
