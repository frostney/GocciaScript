/*---
description: Restricted binding names depend on source strictness
features: [identifiers]
---*/

test("curated default semantics do not make source code strict", () => {
  const eval = 1;
  const arguments = 2;

  expect(eval + arguments).toBe(3);
});
