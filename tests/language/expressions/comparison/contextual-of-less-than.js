/*---
description: Less-than after an "of" identifier is not mistaken for JSX
features: [relational-operators, jsx]
---*/

test("contextual of identifier before less-than stays relational", () => {
  const of = 1;
  const t = 2;
  const result = of<t ? 1 : 0;
  expect(result).toBe(1);
});
