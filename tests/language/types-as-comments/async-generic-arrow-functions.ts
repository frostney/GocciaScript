/*---
description: A bare `<T>` generic async arrow parses in a .ts source, where `<` is always type syntax
features: [types-as-comments, async-await]
---*/

test("bare type parameter list", async () => {
  const wrap = async <T>(value: T): Promise<T[]> => [value];

  expect(await wrap(2)).toEqual([2]);
});
