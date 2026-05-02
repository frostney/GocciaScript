/*---
description: Object property modification and deletion
features: [Object, property-access, object-literals]
---*/

test("object property modification", () => {
  const obj = {
    existing: "value",
    toDelete: "will be deleted",
  };

  // Test property modification
  obj.existing = "modified";
  expect(obj.existing).toBe("modified");
});

test("object property addition", () => {
  const obj = {
    existing: "value",
    toDelete: "will be deleted",
  };

  // Test new property addition
  obj.newProp = "added";
  expect(obj.newProp).toBe("added");
});

test("property assignment on null and undefined reports receiver", () => {
  let nullError;
  let undefinedError;
  let numberError;
  const nullTarget = null;
  const undefinedTarget = undefined;
  const numberTarget = 42;

  try {
    nullTarget.missing = 1;
  } catch (e) {
    nullError = e;
  }

  try {
    undefinedTarget.missing = 1;
  } catch (e) {
    undefinedError = e;
  }

  try {
    numberTarget.missing = 1;
  } catch (e) {
    numberError = e;
  }

  expect(nullError instanceof TypeError).toBe(true);
  expect(nullError.message).toBe("Cannot set properties of null (setting 'missing')");
  expect(undefinedError instanceof TypeError).toBe(true);
  expect(undefinedError.message).toBe("Cannot set properties of undefined (setting 'missing')");
  expect(numberError instanceof TypeError).toBe(true);
  expect(numberError.message).toBe("Cannot set property on non-object");
});

test("compound property assignment on primitive receivers reports receiver", () => {
  let numberError;
  let symbolError;
  const numberTarget = 42;
  const symbolKey = Symbol("missing");
  let directSymbolError;

  try {
    numberTarget.missing += 1;
  } catch (e) {
    numberError = e;
  }

  try {
    numberTarget[symbolKey] += 1;
  } catch (e) {
    symbolError = e;
  }

  try {
    numberTarget[symbolKey] = 1;
  } catch (e) {
    directSymbolError = e;
  }

  expect(numberError instanceof TypeError).toBe(true);
  expect(numberError.message).toBe("Cannot set property on non-object");
  expect(symbolError instanceof TypeError).toBe(true);
  expect(symbolError.message).toBe("Cannot set property on non-object");
  expect(directSymbolError instanceof TypeError).toBe(true);
  expect(directSymbolError.message).toBe("Cannot set property on non-object");
});

test("symbol logical compound assignment reads before deciding to write", () => {
  const inheritedKey = Symbol("primitive-inherited");
  Object.defineProperty(Symbol.prototype, inheritedKey, {
    value: 12,
    configurable: true,
  });

  try {
    const symbolTarget = Symbol("target");
    expect(symbolTarget[inheritedKey] ??= 99).toBe(12);
  } finally {
    Reflect.deleteProperty(Symbol.prototype, inheritedKey);
  }

  const nullTarget = null;
  expect(() => {
    nullTarget[inheritedKey] ??= 1;
  }).toThrow(TypeError);
});

test("object property deletion", () => {
  const obj = {
    existing: "value",
    toDelete: "will be deleted",
  };

  // Test property deletion
  delete obj.toDelete;
  expect(obj.toDelete).toBeUndefined();

  // TODO: `in` operator does not exist
  // expect("toDelete" in obj).toBeFalsy();
});

test("bracket notation with static values", () => {
  const obj = {
    prop1: "value1",
    prop2: "value2",
    "special-key": "special-value",
    123: "numeric-key",
  };

  // Test bracket notation
  expect(obj["prop1"]).toBe("value1");
  expect(obj["special-key"]).toBe("special-value");
  expect(obj[123]).toBe("numeric-key");
  expect(obj["123"]).toBe("numeric-key");

  obj["prop1"] = "new value";
  expect(obj["prop1"]).toBe("new value");

  obj["prop2"] = "new value 2";
  expect(obj["prop2"]).toBe("new value 2");

  obj[123] = "new numeric key";
  expect(obj[123]).toBe("new numeric key");

  obj["123"] = "new numeric key 2";
  expect(obj["123"]).toBe("new numeric key 2");

  obj["special-key"] = "new special key";
  expect(obj["special-key"]).toBe("new special key");

  obj["#price$usd"] = 12;
  expect(obj["#price$usd"]).toBe(12);
});

test("bracket notation with dynamic values", () => {
  const obj = {
    prop1: "value1",
    prop2: "value2",
    "special-key": "special-value",
    123: "numeric-key",
  };

  const key = "prop2";
  expect(obj[key]).toBe("value2");

  obj[key] = "new value";
  expect(obj[key]).toBe("new value");
});
