#!/usr/bin/env goccia

test("leading shebang lines are ignored", () => {
  expect(8 / 2).toBe(4);
  expect(/ab/.test("zabz")).toBe(true);
});
