/*---
description: Module top-level let and const bindings are named in dead-zone and const-assignment errors
features: [modules, temporal-dead-zone]
---*/

const messageOf = (run) => {
  try {
    run();
  } catch (error) {
    return `${error.constructor.name}: ${error.message}`;
  }
  return "no error";
};

// Everything up to the declarations below runs while they are uninitialized.
const earlyRead = messageOf(() => counter);
const earlyIncrement = messageOf(() => {
  counter++;
});
const earlyConstWrite = messageOf(() => {
  limit = 1;
});
let earlyDirectConstWrite = "no error";
try {
  limit = 2;
} catch (error) {
  earlyDirectConstWrite = `${error.constructor.name}: ${error.message}`;
}

let counter = 0;
const limit = 16;

describe("module top-level binding error messages", () => {
  test("reads and writes before initialization name the binding", () => {
    expect(earlyRead).toBe("ReferenceError: Cannot access 'counter' before initialization");
    expect(earlyIncrement).toBe("ReferenceError: Cannot access 'counter' before initialization");
  });

  test("an assignment to a const in its dead zone throws the ReferenceError", () => {
    expect(earlyConstWrite).toBe("ReferenceError: Cannot access 'limit' before initialization");
    expect(earlyDirectConstWrite).toBe("ReferenceError: Cannot access 'limit' before initialization");
  });

  test("an assignment to an initialized const names it", () => {
    expect(messageOf(() => {
      limit = 5;
    })).toBe("TypeError: Assignment to constant variable 'limit'");
    expect(messageOf(() => {
      limit += 1;
    })).toBe("TypeError: Assignment to constant variable 'limit'");
  });
});
