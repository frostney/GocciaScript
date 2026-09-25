/*---
description: Template literal types are parsed and ignored at runtime
features: [types-as-comments]
---*/

describe("template literal types", () => {

test("variable annotation with a substitution", () => {
  const id: `id-${number}` = "id-42";

  expect(id).toBe("id-42");
});

test("as-expression position", () => {
  const id = "id-42" as `id-${number}`;

  expect(id).toBe("id-42");
});

test("template type without substitutions", () => {
  const tag: `fixed` = "fixed";

  expect(tag).toBe("fixed");
});

test("multiple substitutions", () => {
  const key: `${string}-${number}` = "a-1";

  expect(key).toBe("a-1");
});

test("union with a template literal member", () => {
  const value: `on-${string}` | null = "on-click";

  expect(value).toBe("on-click");
});

test("parameter and return annotations", () => {
  const echo = (input: `v${number}`): `v${number}` => input;

  expect(echo("v1")).toBe("v1");
});

test("nested template type inside a substitution", () => {
  const nested: `a-${`b-${string}`}` = "a-b-c";

  expect(nested).toBe("a-b-c");
});

test("template literal expressions are unaffected", () => {
  const n = 42;
  const label: `id-${number}` = "id-7";

  expect(`id-${n}`).toBe("id-42");
  expect(`${label}!`).toBe("id-7!");
});

test("statements after a template literal type still parse", () => {
  const first: `id-${number}` = "id-1";
  const second = 2;

  expect(first).toBe("id-1");
  expect(second).toBe(2);
});

});
