/*---
description: Number operation benchmarks
---*/

suite("number creation", () => {
  bench("integer arithmetic", () => {
    const a = 42;
    const b = 17;
    const sum = a + b;
    const diff = a - b;
    const prod = a * b;
    const quot = a / b;
  });

  bench("floating point arithmetic", () => {
    const a = 3.14159;
    const b = 2.71828;
    const sum = a + b;
    const prod = a * b;
    const quot = a / b;
  });

  bench("number coercion", () => {
    const a = Number("42");
    const b = Number("3.14");
    const c = Number(true);
    const d = Number(false);
    const e = Number(null);
  });
});

suite("number prototype methods", () => {
  bench("toFixed", () => {
    const num = 3.14159;
    const a = num.toFixed(0);
    const b = num.toFixed(2);
    const c = num.toFixed(5);
  });

  bench("toString", () => {
    const num = 255;
    const a = num.toString();
    const b = num.toString(16);
    const c = num.toString(10);
  });

  bench("valueOf", () => {
    const a = (42).valueOf();
    const b = (3.14).valueOf();
    const c = (0).valueOf();
  });

  bench("toPrecision", () => {
    const num = 123.456;
    const a = num.toPrecision();
    const b = num.toPrecision(4);
    const c = num.toPrecision(2);
  });
});

suite("number static methods", () => {
  bench("Number.isNaN", () => {
    const a = Number.isNaN(NaN);
    const b = Number.isNaN(42);
    const c = Number.isNaN("hello");
  });

  bench("Number.isFinite", () => {
    const a = Number.isFinite(42);
    const b = Number.isFinite(Infinity);
    const c = Number.isFinite(NaN);
  });

  bench("Number.isInteger", () => {
    const a = Number.isInteger(42);
    const b = Number.isInteger(3.14);
    const c = Number.isInteger(0);
  });

  bench("Number.parseInt and parseFloat", () => {
    const a = Number.parseInt("42");
    const b = Number.parseFloat("3.14");
    const c = Number.parseInt("ff", 16);
  });
});
