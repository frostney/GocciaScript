/*---
description: A compound assignment to a module's top-level const applies its operator before it throws the assignment TypeError
features: [modules, const, compound-assignment-operators, logical-assignment-operators]
---*/

const errorName = (run) => {
  try {
    run();
    return "none";
  } catch (error) {
    return error.constructor.name;
  }
};

// Runs while MODULE_LIMIT is still uninitialized.
let deadZoneUpdate = "none";
try {
  MODULE_LIMIT += 1;
} catch (error) {
  deadZoneUpdate = error.constructor.name;
}

const MODULE_LIMIT = 5;

const conversions = [];
const MODULE_OBJECT = {
  valueOf() {
    conversions.push("valueOf");
    return 1;
  },
};
export const EXPORTED_OBJECT = {
  valueOf() {
    conversions.push("exported");
    return 1;
  },
};

let objectUpdate = "none";
try {
  MODULE_OBJECT -= 1;
} catch (error) {
  objectUpdate = error.constructor.name;
}
let exportedUpdate = "none";
try {
  EXPORTED_OBJECT >>= 1;
} catch (error) {
  exportedUpdate = error.constructor.name;
}
let shortCircuited = "none";
try {
  MODULE_LIMIT ||= 1;
} catch (error) {
  shortCircuited = error.constructor.name;
}

describe("compound assignment to top-level module consts", () => {
  test("an assignment before the declaration throws ReferenceError", () => {
    expect(deadZoneUpdate).toBe("ReferenceError");
  });

  test("a const converts its operand once before it throws TypeError", () => {
    expect(objectUpdate).toBe("TypeError");
    expect(exportedUpdate).toBe("TypeError");
    expect(conversions).toEqual(["valueOf", "exported"]);
  });

  test("a function converts the operand once as well", () => {
    expect(
      errorName(() => {
        MODULE_OBJECT *= 2;
      }),
    ).toBe("TypeError");
    expect(conversions).toEqual(["valueOf", "exported", "valueOf"]);
  });

  test("a short-circuited logical assignment does not throw", () => {
    expect(shortCircuited).toBe("none");
    expect(MODULE_LIMIT).toBe(5);
  });
});
