/*---
description: await remains an identifier in script positions that are not await expressions
features: [async-await]
---*/

let await = 1;
const __gocciaAwaitIdentifierInitial = await;
await = 2;
const __gocciaAwaitIdentifierUpdated = await;

test("await can be used as a script identifier", () => {
  expect(__gocciaAwaitIdentifierInitial).toBe(1);
  expect(__gocciaAwaitIdentifierUpdated).toBe(2);
});

test("await in a class field initializer of an async function is an identifier", async () => {
  const makeClass = async () => class {
    x = await;
  };
  const C = await makeClass();
  expect(new C().x).toBe(2);
});
