/*---
description: RegExp constructor, prototype, and string integration benchmarks
---*/

import { bench, group } from "goccia:microbench";

group("RegExp construction", () => {
  bench("regex literal creation", () => {
    const regex = /foo(bar)?/gi;
  });

  bench("new RegExp(pattern, flags)", () => {
    const regex = new RegExp("foo(bar)?", "gi");
  });

  bench("RegExp(existingRegex) returns the same regex", ({ *setup() {
    const regex = (() => /foo(bar)?/gi)();
    yield () => {
      const result = RegExp(regex);
    };
  } }).setup);
});

group("RegExp prototype methods", () => {
  bench("test() on a global regex", ({ *setup() {
    const { regex, input } = (() => ({
      regex: /foo/g,
      input: "foo bar foo baz foo",
    }))();
    yield () => {
      regex.lastIndex = 0;
      const matched = regex.test(input);
    };
  } }).setup);

  bench("exec() with capture groups", ({ *setup() {
    const { regex, input } = (() => ({
      regex: /(foo)(bar)?/g,
      input: "foo foobar foo",
    }))();
    yield () => {
      regex.lastIndex = 0;
      const match = regex.exec(input);
    };
  } }).setup);

  bench("toString()", ({ *setup() {
    const regex = (() => /foo(bar)?/gim)();
    yield () => {
      const value = regex.toString();
    };
  } }).setup);
});

group("String integration with RegExp", () => {
  bench("match() with global regex", ({ *setup() {
    const input = (() => "foo bar foo baz foo")();
    yield () => {
      const matches = input.match(/foo/g);
    };
  } }).setup);

  bench("match() with many global results", ({ *setup() {
    const input = (() => "foo ".repeat(256))();
    yield () => {
      const matches = input.match(/foo/g);
    };
  } }).setup);

  bench("matchAll() with capture groups", ({ *setup() {
    const input = (() => "foo1 foo2 foo3")();
    yield () => {
      const results = [];
      for (const match of input.matchAll(/(foo)([0-9])/g)) {
        results.push(match[1]);
      }
    };
  } }).setup);

  bench("replace() with global regex", ({ *setup() {
    const input = (() => "foo bar foo baz foo")();
    yield () => {
      const replaced = input.replace(/foo/g, "qux");
    };
  } }).setup);

  bench("replace() with many global results", ({ *setup() {
    const input = (() => "foo ".repeat(256))();
    yield () => {
      const replaced = input.replace(/foo/g, "qux");
    };
  } }).setup);

  bench("search() with regex", ({ *setup() {
    const input = (() => "alpha beta gamma delta")();
    yield () => {
      const index = input.search(/gamma/);
    };
  } }).setup);

  bench("split() with regex separator", ({ *setup() {
    const input = (() => "foo,bar;baz,qux")();
    yield () => {
      const parts = input.split(/[;,]/);
    };
  } }).setup);
});
