/*---
description: Temporal operation benchmarks
---*/

import { bench, group } from "goccia:microbench";

group("Temporal.PlainDate arithmetic", () => {
  bench("PlainDate.add({ months: 1 })", ({ *setup() {
    const date = (() => Temporal.PlainDate.from("2024-01-31"))();
    yield () => {
      const next = date.add({ months: 1 });
    };
  } }).setup);

  bench("PlainDate.until(..., { largestUnit: 'months' })", ({ *setup() {
    const ctx = (() => ({
      start: Temporal.PlainDate.from("2024-01-01"),
      end: Temporal.PlainDate.from("2025-07-15"),
    }))();
    yield () => {
      const duration = ctx.start.until(ctx.end, { largestUnit: "months" });
    };
  } }).setup);
});

group("Temporal.Duration balancing", () => {
  bench("Duration.total days relative to PlainDate", ({ *setup() {
    const duration = (() => Temporal.Duration.from({ years: 1, months: 6, days: 3 }))();
    yield () => {
      const days = duration.total({ unit: "days", relativeTo: "2024-01-01" });
    };
  } }).setup);

  bench("Duration.round to hours", ({ *setup() {
    const duration = (() => Temporal.Duration.from({ hours: 1, minutes: 45, seconds: 30 }))();
    yield () => {
      const rounded = duration.round({ smallestUnit: "hours", roundingMode: "halfExpand" });
    };
  } }).setup);
});

group("Temporal.ZonedDateTime", () => {
  bench("ZonedDateTime.from named time zone", () => {
    const zdt = Temporal.ZonedDateTime.from("2024-03-10T12:30:00-04:00[America/New_York]");
  });

  bench("ZonedDateTime.since across DST", ({ *setup() {
    const ctx = (() => ({
      start: Temporal.ZonedDateTime.from("2024-03-09T00:00:00-05:00[America/New_York]"),
      end: Temporal.ZonedDateTime.from("2024-03-10T12:30:00-04:00[America/New_York]"),
    }))();
    yield () => {
      const duration = ctx.end.since(ctx.start, { largestUnit: "days" });
    };
  } }).setup);
});
