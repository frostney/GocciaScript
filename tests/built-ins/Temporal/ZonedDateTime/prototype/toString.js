/*---
description: Temporal.ZonedDateTime.prototype.toString
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.ZonedDateTime.prototype.toString", () => {
  test("toString includes timezone annotation", () => {
    const epochNs = 1710510330000n * 1000000n;
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const s = zdt.toString();
    expect(s).toContain("[UTC]");
    expect(s).toContain("2024-03-15");
    expect(s).toContain("13:45:30");
  });

  test("rounding recomputes UTC offset after day carry", () => {
    // 2024-03-15T23:59:59.500Z — rounding to seconds with halfExpand produces
    // a carry into the next day. The offset must be recomputed from the rounded
    // instant so the formatted string remains consistent.
    const epochNs = 1710547199500n * 1000000n; // 2024-03-15T23:59:59.500Z
    const zdt = new Temporal.ZonedDateTime(epochNs, "UTC");
    const s = zdt.toString({ fractionalSecondDigits: 0, roundingMode: "halfExpand" });
    expect(s).toBe("2024-03-16T00:00:00+00:00[UTC]");
  });

  test("rounding with fixed-offset timezone recomputes correctly", () => {
    // 2024-03-15T23:59:59.500 in +05:30 — rounding to seconds carries into next day
    // Epoch: local 23:59:59.500 +05:30 → UTC 18:29:59.500
    const epochNs = 1710527399500n * 1000000n; // UTC 2024-03-15T18:29:59.500
    const zdt = new Temporal.ZonedDateTime(epochNs, "+05:30");
    const s = zdt.toString({ fractionalSecondDigits: 0, roundingMode: "halfExpand" });
    expect(s).toBe("2024-03-16T00:00:00+05:30[+05:30]");
  });

  test("no-op rounding preserves original offset from epoch", () => {
    // When no rounding occurs (auto precision), the offset must come from the
    // original epoch, not from re-resolving local fields through LocalToEpochMs.
    // This prevents fold-flip for ambiguous wall-clock times during DST fall-back.
    const epochNs = 1710510330000n * 1000000n; // 2024-03-15T13:45:30Z
    const zdt = new Temporal.ZonedDateTime(epochNs, "+05:30");
    const auto = zdt.toString(); // auto precision — no rounding
    expect(auto).toContain("+05:30");
    expect(auto).toContain("[+05:30]");
    const nano = zdt.toString({ fractionalSecondDigits: 9 }); // nanosecond — no rounding
    expect(nano).toContain("+05:30");
    expect(nano).toContain("[+05:30]");
  });
});
