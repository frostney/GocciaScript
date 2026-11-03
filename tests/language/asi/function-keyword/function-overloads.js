/*---
description: Overload signatures end at their line break under ASI
features: [compat-asi, types-as-comments]
---*/

export function pick(text: string): string
export function pick(count: number): number
export function pick(value: string | number) {
  return value
}

function wrap(
  value: string,
): string[]
function wrap(input: string) {
  return [input]
}

class Store {
  get(name: string): string
  get(index: number): string
  get(key: string | number) {
    return `got ${key}`
  }
}

test("function overloads without semicolons are erased", () => {
  expect(pick(1)).toBe(1)
  expect(wrap("a")).toEqual(["a"])
})

test("class method overloads without semicolons are erased", () => {
  expect(new Store().get("k")).toBe("got k")
})

test("a function body on its own line is still the body", () => {
  function later(value: number): number
  {
    return value + 1
  }
  expect(later(1)).toBe(2)
})
