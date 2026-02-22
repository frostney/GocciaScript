/*---
description: Enums can be exported and imported across modules
features: [enum-declaration, modules]
---*/

import { Direction } from "./helpers/direction-enum.js";

test("imported enum members are accessible", () => {
  expect(Direction.Up).toBe(0);
  expect(Direction.Down).toBe(1);
  expect(Direction.Left).toBe(2);
  expect(Direction.Right).toBe(3);
});

test("imported enum is iterable", () => {
  const entries = [...Direction];
  expect(entries.length).toBe(4);
  expect(entries[0][0]).toBe("Up");
  expect(entries[0][1]).toBe(0);
});

test("imported enum has Symbol.toStringTag", () => {
  expect(Direction[Symbol.toStringTag]).toBe("Direction");
});
