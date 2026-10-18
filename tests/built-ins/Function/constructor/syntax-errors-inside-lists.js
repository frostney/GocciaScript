/*---
description: A syntax error in the middle of a list, block, pattern, class body or match the parser is building throws a SyntaxError
features: [Function, unsafe-function-constructor, pattern-matching]
---*/

// Each body stops inside a container the parser is still filling (array
// elements, block statements, template parts, arguments, class members,
// destructuring and match patterns, switch cases). The memory-safety job runs
// this file under heaptrc, so a container a failed parse does not free shows
// up there as an unfreed block.
const bodies = [
  "[1,",
  "{",
  "`a${",
  "f`a${1",
  "new F(1,",
  "f(1, 2",
  "class A { + }",
  "class A { m() {} static s = 1; get g() {} + }",
  "let [a, = 1;",
  "let {a, = 1;",
  "({a: 1} = 1);",
  "([a, 1] = 1);",
  "switch (1) { x }",
  "switch (1) { case 1: x; y z }",
  "({a: 1, b",
  "match (1) { [1, : 2 }",
  "match (1) { {a: 1, : 2 }",
  "match (1) { 1 and : 2 }",
  "match (1) { 1 or : 2 }",
  "match (1) { F(1, : 2 }",
  "match (1) { 1: 2; 3 }",
];

describe("syntax errors inside a container the parser is building", () => {
  for (const body of bodies) {
    test(JSON.stringify(body), () => {
      expect(() => new Function(body)).toThrow(SyntaxError);
    });
  }
});
