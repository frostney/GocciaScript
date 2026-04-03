/*---
description: RegExp constructor, prototype, and string integration benchmarks
---*/

suite("RegExp construction", () => {
  bench("regex literal creation", {
    run: () => {
      const regex = /foo(bar)?/gi;
    },
  });

  bench("new RegExp(pattern, flags)", {
    run: () => {
      const regex = new RegExp("foo(bar)?", "gi");
    },
  });

  bench("RegExp(existingRegex) returns the same regex", {
    setup: () => /foo(bar)?/gi,
    run: (regex) => {
      const result = RegExp(regex);
    },
  });
});

suite("RegExp prototype methods", () => {
  bench("test() on a global regex", {
    setup: () => ({
      regex: /foo/g,
      input: "foo bar foo baz foo",
    }),
    run: ({ regex, input }) => {
      regex.lastIndex = 0;
      const matched = regex.test(input);
    },
  });

  bench("exec() with capture groups", {
    setup: () => ({
      regex: /(foo)(bar)?/g,
      input: "foo foobar foo",
    }),
    run: ({ regex, input }) => {
      regex.lastIndex = 0;
      const match = regex.exec(input);
    },
  });

  bench("toString()", {
    setup: () => /foo(bar)?/gim,
    run: (regex) => {
      const value = regex.toString();
    },
  });
});

suite("String integration with RegExp", () => {
  bench("match() with global regex", {
    setup: () => "foo bar foo baz foo",
    run: (input) => {
      const matches = input.match(/foo/g);
    },
  });

  bench("matchAll() with capture groups", {
    setup: () => "foo1 foo2 foo3",
    run: (input) => {
      const results = [];
      for (const match of input.matchAll(/(foo)(\d)/g)) {
        results.push(match[1]);
      }
    },
  });

  bench("replace() with global regex", {
    setup: () => "foo bar foo baz foo",
    run: (input) => {
      const replaced = input.replace(/foo/g, "qux");
    },
  });

  bench("search() with regex", {
    setup: () => "alpha beta gamma delta",
    run: (input) => {
      const index = input.search(/gamma/);
    },
  });

  bench("split() with regex separator", {
    setup: () => "foo,bar;baz,qux",
    run: (input) => {
      const parts = input.split(/[;,]/);
    },
  });
});
