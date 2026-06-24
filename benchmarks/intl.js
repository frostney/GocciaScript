/*---
description: Intl operation benchmarks
---*/

suite("Intl.NumberFormat", () => {
  bench("format decimal", {
    setup: () => new Intl.NumberFormat("en-US", { maximumFractionDigits: 2 }),
    run: (format) => {
      const result = format.format(1234567.89);
    },
  });

  bench("format currency", {
    setup: () => new Intl.NumberFormat("de-DE", { style: "currency", currency: "EUR" }),
    run: (format) => {
      const result = format.format(1234567.89);
    },
  });
});

suite("Intl.DateTimeFormat", () => {
  bench("format UTC date", {
    setup: () => ({
      format: new Intl.DateTimeFormat("en-US", {
        dateStyle: "medium",
        timeStyle: "short",
        timeZone: "UTC",
      }),
      date: new Date(1710510330000),
    }),
    run: (ctx) => {
      const result = ctx.format.format(ctx.date);
    },
  });

  bench("formatRange UTC dates", {
    setup: () => ({
      format: new Intl.DateTimeFormat("en-US", {
        dateStyle: "medium",
        timeStyle: "short",
        timeZone: "UTC",
      }),
      start: new Date(1710510330000),
      end: new Date(1710513930000),
    }),
    run: (ctx) => {
      const result = ctx.format.formatRange(ctx.start, ctx.end);
    },
  });
});

suite("Intl.Collator", () => {
  bench("compare numeric strings", {
    setup: () => new Intl.Collator("en", { numeric: true }),
    run: (collator) => {
      const result = collator.compare("item 9", "item 10");
    },
  });

  bench("sort short string list", {
    setup: () => ({
      collator: new Intl.Collator("en", { sensitivity: "base" }),
      values: ["zebra", "angstrom", "apple", "Algebra", "banana"],
    }),
    run: (ctx) => {
      const sorted = ctx.values.slice().sort(ctx.collator.compare);
    },
  });
});
