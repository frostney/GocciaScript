/*---
description: Nullish property access names negative-zero keys consistently across value representations
esid: sec-getvalue
features: []
---*/

// Diagnostic wording is an engine contract. Number::toString spells -0 as "0",
// but formatting the diagnostic must not run guest key-coercion hooks.
const messageFrom = (operation) => {
  let error;
  try {
    operation();
  } catch (caught) {
    error = caught;
  }
  expect(error instanceof TypeError).toBe(true);
  return error.message;
};

for (const base of [null, undefined]) {
  const readMessage = `Cannot read properties of ${base} (reading '0')`;

  test(`${base} reads name array-element negative zero like literal and const keys`, () => {
    const key = -0;
    expect(messageFrom(() => base[-0])).toBe(readMessage);
    expect(messageFrom(() => base[key])).toBe(readMessage);

    const keys = [-0, 0, 1, 1.5, NaN, "s", true, null];
    const names = ["0", "0", "1", "1.5", "NaN", "s", "true", "null"];
    // Check the already-working primitive keys before the regression assertion.
    for (const index of [1, 2, 3, 4, 5, 6, 7, 0]) {
      expect(messageFrom(() => base[keys[index]])).toBe(
        `Cannot read properties of ${base} (reading '${names[index]}')`
      );
    }
  });

  test(`${base} reads name negative zero obtained by for-of`, () => {
    for (const key of [-0]) {
      expect(messageFrom(() => base[key])).toBe(readMessage);
    }
  });

  test(`${base} reads name negative zero returned through an array`, () => {
    const identity = (value) => [value][0];
    expect(messageFrom(() => base[identity(-0)])).toBe(readMessage);
  });

  test(`${base} writes name an array-element negative-zero key`, () => {
    const keys = [-0];
    expect(messageFrom(() => { base[keys[0]] = 1; })).toBe(
      `Cannot set properties of ${base} (setting '0')`
    );
  });

  test(`${base} deletes name an array-element negative-zero key`, () => {
    const keys = [-0];
    expect(messageFrom(() => { delete base[keys[0]]; })).toBe(readMessage);
  });
}
