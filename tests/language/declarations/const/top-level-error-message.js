/*---
description: Top-level let and const bindings are named in dead-zone and const-assignment errors
features: [let, const-declaration, temporal-dead-zone]
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
const earlyRead = messageOf(() => COUNTER);
const earlyIncrement = messageOf(() => {
  COUNTER++;
});
const earlyCompound = messageOf(() => {
  COUNTER += 1;
});
const earlyConstWrite = messageOf(() => {
  LIMIT = 1;
});
let earlyDirectConstWrite = "no error";
try {
  LIMIT = 2;
} catch (error) {
  earlyDirectConstWrite = `${error.constructor.name}: ${error.message}`;
}

let COUNTER = 0;
const LIMIT = 16;

let lateDirectConstWrite = "no error";
try {
  LIMIT += 1;
} catch (error) {
  lateDirectConstWrite = `${error.constructor.name}: ${error.message}`;
}

describe("top-level binding error messages", () => {
  test("reads and writes before initialization name the binding", () => {
    expect(earlyRead).toBe("ReferenceError: Cannot access 'COUNTER' before initialization");
    expect(earlyIncrement).toBe("ReferenceError: Cannot access 'COUNTER' before initialization");
    expect(earlyCompound).toBe("ReferenceError: Cannot access 'COUNTER' before initialization");
  });

  test("an assignment to a const in its dead zone throws the ReferenceError", () => {
    expect(earlyConstWrite).toBe("ReferenceError: Cannot access 'LIMIT' before initialization");
    expect(earlyDirectConstWrite).toBe("ReferenceError: Cannot access 'LIMIT' before initialization");
  });

  test("an assignment to an initialized const names it", () => {
    expect(lateDirectConstWrite).toBe("TypeError: Assignment to constant variable 'LIMIT'");
    expect(messageOf(() => {
      LIMIT = 5;
    })).toBe("TypeError: Assignment to constant variable 'LIMIT'");
    expect(LIMIT).toBe(16);
  });
});
