/*---
description: String operation benchmarks
---*/

suite("string creation", () => {
  bench("string concatenation", {
    run: () => {
      const result = "hello" + " " + "world" + "!" + " " + "foo" + " " + "bar";
    },
  });

  bench("template literal", {
    run: () => {
      const name = "world";
      const greeting = `hello ${name}! welcome to ${name}`;
    },
  });

  bench("string repeat", {
    run: () => {
      const result = "abc".repeat(50);
    },
  });
});

suite("string methods", () => {
  bench("split and join", {
    run: () => {
      const str = "one,two,three,four,five,six,seven,eight,nine,ten";
      const parts = str.split(",");
      const joined = parts.join("-");
    },
  });

  bench("indexOf and includes", {
    run: () => {
      const str = "the quick brown fox jumps over the lazy dog";
      const a = str.indexOf("fox");
      const b = str.includes("lazy");
      const c = str.indexOf("cat");
    },
  });

  bench("toUpperCase and toLowerCase", {
    run: () => {
      const str = "Hello World From GocciaScript";
      const upper = str.toUpperCase();
      const lower = str.toLowerCase();
    },
  });

  bench("slice and substring", {
    run: () => {
      const str = "abcdefghijklmnopqrstuvwxyz";
      const a = str.slice(5, 15);
      const b = str.substring(10, 20);
      const c = str.slice(-5);
    },
  });

  bench("trim operations", {
    run: () => {
      const str = "   hello world   ";
      const a = str.trim();
      const b = str.trimStart();
      const c = str.trimEnd();
    },
  });

  bench("replace and replaceAll", {
    run: () => {
      const str = "foo bar foo baz foo";
      const a = str.replace("foo", "qux");
      const b = str.replaceAll("foo", "qux");
    },
  });

  bench("startsWith and endsWith", {
    run: () => {
      const str = "hello world";
      const a = str.startsWith("hello");
      const b = str.endsWith("world");
      const c = str.startsWith("world");
      const d = str.endsWith("hello");
    },
  });

  bench("padStart and padEnd", {
    run: () => {
      const str = "42";
      const a = str.padStart(8, "0");
      const b = str.padEnd(8, ".");
    },
  });
});

suite("tagged templates", () => {
  // Baseline: identity tag — measures pure tag dispatch + template object construction cost
  const identity = (strings) => strings[0];

  bench("identity tag, no substitutions", {
    run: () => {
      const result = identity`hello world`;
    },
  });

  // Compare tag dispatch against equivalent plain function call
  const join = (strings, ...values) =>
    strings.reduce((acc, str, i) => acc + str + (values[i] !== undefined ? String(values[i]) : ""), "");

  bench("tag with 1 substitution", {
    run: () => {
      const x = 42;
      const result = join`value: ${x}`;
    },
  });

  bench("tag with 3 substitutions", {
    run: () => {
      const a = 1;
      const b = 2;
      const c = 3;
      const result = join`${a} + ${b} = ${c}`;
    },
  });

  bench("tag with 6 substitutions", {
    run: () => {
      const a = 1;
      const b = 2;
      const c = 3;
      const d = 4;
      const e = 5;
      const f = 6;
      const result = join`${a}-${b}-${c}-${d}-${e}-${f}`;
    },
  });

  // String.raw: measures the built-in tag path
  bench("String.raw, no substitutions", {
    run: () => {
      const result = String.raw`hello\nworld\ttab`;
    },
  });

  bench("String.raw, 2 substitutions", {
    run: () => {
      const a = "foo";
      const b = "bar";
      const result = String.raw`prefix\t${a}\n${b}\tsuffix`;
    },
  });

  // Accessing .raw inside tag — measures template object property access
  const rawJoin = (strings) =>
    strings.raw.reduce((acc, str, i) =>
      acc + str + (i < strings.raw.length - 1 ? "|" : ""), "");

  bench("tag accessing .raw array", {
    run: () => {
      const result = rawJoin`line\none\ttwo\\three`;
    },
  });

  // Method as tag — measures this-binding through member expression tag
  const formatter = {
    prefix: "[LOG] ",
    tag(strings, ...values) {
      return this.prefix + strings.reduce(
        (acc, str, i) => acc + str + (values[i] !== undefined ? String(values[i]) : ""), ""
      );
    },
  };

  bench("method as tag (this binding)", {
    run: () => {
      const level = "info";
      const msg = "started";
      const result = formatter.tag`${level}: ${msg}`;
    },
  });
});
