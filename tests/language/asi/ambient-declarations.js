/*---
description: an ambient declaration ends at its line break under ASI, and a `declare` alone on its line is an identifier
features: [compat-asi, types-as-comments]
---*/

declare const env: {
  mode: string
}
declare module "side-effect"
declare function greet(
  name: string
): void
const declare = 40
const first = declare
const after = first + 2

test("ambient declarations without semicolons end at their line break", () => {
  expect(typeof env).toBe("undefined")
  expect(typeof greet).toBe("undefined")
  expect(after).toBe(42)
})

test("declare followed by a line break is an identifier statement", () => {
  let declare = 1
  declare
  const value = 2
  expect(declare + value).toBe(3)
})
