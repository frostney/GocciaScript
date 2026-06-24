/*---
description: Temporal operation benchmarks
---*/

suite("Temporal.PlainDate arithmetic", () => {
  bench("PlainDate.add({ months: 1 })", {
    setup: () => Temporal.PlainDate.from("2024-01-31"),
    run: (date) => {
      const next = date.add({ months: 1 });
    },
  });

  bench("PlainDate.until(..., { largestUnit: 'months' })", {
    setup: () => ({
      start: Temporal.PlainDate.from("2024-01-01"),
      end: Temporal.PlainDate.from("2025-07-15"),
    }),
    run: (ctx) => {
      const duration = ctx.start.until(ctx.end, { largestUnit: "months" });
    },
  });
});

suite("Temporal.Duration balancing", () => {
  bench("Duration.total days relative to PlainDate", {
    setup: () => Temporal.Duration.from({ years: 1, months: 6, days: 3 }),
    run: (duration) => {
      const days = duration.total({ unit: "days", relativeTo: "2024-01-01" });
    },
  });

  bench("Duration.round to hours", {
    setup: () => Temporal.Duration.from({ hours: 1, minutes: 45, seconds: 30 }),
    run: (duration) => {
      const rounded = duration.round({ smallestUnit: "hours", roundingMode: "halfExpand" });
    },
  });
});

suite("Temporal.ZonedDateTime", () => {
  bench("ZonedDateTime.from named time zone", {
    run: () => {
      const zdt = Temporal.ZonedDateTime.from("2024-03-10T12:30:00-04:00[America/New_York]");
    },
  });

  bench("ZonedDateTime.since across DST", {
    setup: () => ({
      start: Temporal.ZonedDateTime.from("2024-03-09T00:00:00-05:00[America/New_York]"),
      end: Temporal.ZonedDateTime.from("2024-03-10T12:30:00-04:00[America/New_York]"),
    }),
    run: (ctx) => {
      const duration = ctx.end.since(ctx.start, { largestUnit: "days" });
    },
  });
});
