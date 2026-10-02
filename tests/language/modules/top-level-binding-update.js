/*---
description: An update expression on a module's top-level binding throws ReferenceError until the declaration has run
features: [modules, temporal-dead-zone, update-expressions]
---*/

const errorName = (run) => {
  try {
    run();
    return "none";
  } catch (error) {
    return error.constructor.name;
  }
};

// Everything up to the declarations below runs while they are uninitialized.
let letUpdate = "none";
try {
  moduleCounter++;
} catch (error) {
  letUpdate = error.constructor.name;
}
let letUpdateWithResult = "none";
try {
  const result = --moduleCounter;
  letUpdateWithResult = `no error: ${result}`;
} catch (error) {
  letUpdateWithResult = error.constructor.name;
}
let constUpdate = "none";
try {
  MODULE_LIMIT++;
} catch (error) {
  constUpdate = error.constructor.name;
}
const letUpdateFromFunction = errorName(() => {
  moduleCounter--;
});
const constUpdateFromFunction = errorName(() => {
  ++MODULE_LIMIT;
});

let moduleCounter = 5;
const MODULE_LIMIT = 5;

describe("update expressions on top-level module bindings", () => {
  test("an update before the declaration throws ReferenceError", () => {
    expect(letUpdate).toBe("ReferenceError");
    expect(letUpdateWithResult).toBe("ReferenceError");
    expect(constUpdate).toBe("ReferenceError");
    expect(letUpdateFromFunction).toBe("ReferenceError");
    expect(constUpdateFromFunction).toBe("ReferenceError");
  });

  test("an update after the declaration changes a let and rejects a const", () => {
    expect(moduleCounter++).toBe(5);
    expect(--moduleCounter).toBe(5);
    expect(
      errorName(() => {
        moduleCounter++;
      }),
    ).toBe("none");
    expect(moduleCounter).toBe(6);
    expect(
      errorName(() => {
        MODULE_LIMIT++;
      }),
    ).toBe("TypeError");
    expect(MODULE_LIMIT).toBe(5);
  });
});
