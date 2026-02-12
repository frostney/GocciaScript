/*---
description: String operation benchmarks
---*/

suite("string creation", () => {
  bench("string concatenation", () => {
    const result = "hello" + " " + "world" + "!" + " " + "foo" + " " + "bar";
  });

  bench("template literal", () => {
    const name = "world";
    const greeting = `hello ${name}! welcome to ${name}`;
  });

  bench("string repeat", () => {
    const result = "abc".repeat(50);
  });
});

suite("string methods", () => {
  bench("split and join", () => {
    const str = "one,two,three,four,five,six,seven,eight,nine,ten";
    const parts = str.split(",");
    const joined = parts.join("-");
  });

  bench("indexOf and includes", () => {
    const str = "the quick brown fox jumps over the lazy dog";
    const a = str.indexOf("fox");
    const b = str.includes("lazy");
    const c = str.indexOf("cat");
  });

  bench("toUpperCase and toLowerCase", () => {
    const str = "Hello World From GocciaScript";
    const upper = str.toUpperCase();
    const lower = str.toLowerCase();
  });

  bench("slice and substring", () => {
    const str = "abcdefghijklmnopqrstuvwxyz";
    const a = str.slice(5, 15);
    const b = str.substring(10, 20);
    const c = str.slice(-5);
  });

  bench("trim operations", () => {
    const str = "   hello world   ";
    const a = str.trim();
    const b = str.trimStart();
    const c = str.trimEnd();
  });

  bench("replace and replaceAll", () => {
    const str = "foo bar foo baz foo";
    const a = str.replace("foo", "qux");
    const b = str.replaceAll("foo", "qux");
  });

  bench("startsWith and endsWith", () => {
    const str = "hello world";
    const a = str.startsWith("hello");
    const b = str.endsWith("world");
    const c = str.startsWith("world");
    const d = str.endsWith("hello");
  });

  bench("padStart and padEnd", () => {
    const str = "42";
    const a = str.padStart(8, "0");
    const b = str.padEnd(8, ".");
  });
});
