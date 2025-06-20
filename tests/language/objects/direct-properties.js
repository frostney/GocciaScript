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
