/*---
description: Intl operation benchmarks
---*/

import { bench, group } from "goccia:microbench";

group("Intl.NumberFormat", () => {
  bench("format decimal", ({ *setup() {
    const format = (() => new Intl.NumberFormat("en-US", { maximumFractionDigits: 2 }))();
    yield () => {
      const result = format.format(1234567.89);
    };
  } }).setup);

  bench("format currency", ({ *setup() {
    const format = (() => new Intl.NumberFormat("de-DE", { style: "currency", currency: "EUR" }))();
    yield () => {
      const result = format.format(1234567.89);
    };
  } }).setup);
});

group("Intl.DateTimeFormat", () => {
  bench("format UTC date", ({ *setup() {
    const ctx = (() => ({
      format: new Intl.DateTimeFormat("en-US", {
        dateStyle: "medium",
        timeStyle: "short",
        timeZone: "UTC",
      }),
      date: new Date(1710510330000),
    }))();
    yield () => {
      const result = ctx.format.format(ctx.date);
    };
  } }).setup);

  bench("formatRange UTC dates", ({ *setup() {
    const ctx = (() => ({
      format: new Intl.DateTimeFormat("en-US", {
        dateStyle: "medium",
        timeStyle: "short",
        timeZone: "UTC",
      }),
      start: new Date(1710510330000),
      end: new Date(1710513930000),
    }))();
    yield () => {
      const result = ctx.format.formatRange(ctx.start, ctx.end);
    };
  } }).setup);
});

group("Intl.Collator", () => {
  bench("compare numeric strings", ({ *setup() {
    const collator = (() => new Intl.Collator("en", { numeric: true }))();
    yield () => {
      const result = collator.compare("item 9", "item 10");
    };
  } }).setup);

  bench("sort short string list", ({ *setup() {
    const ctx = (() => ({
      collator: new Intl.Collator("en", { sensitivity: "base" }),
      values: ["zebra", "angstrom", "apple", "Algebra", "banana"],
    }))();
    yield () => {
      const sorted = ctx.values.slice().sort(ctx.collator.compare);
    };
  } }).setup);
});
