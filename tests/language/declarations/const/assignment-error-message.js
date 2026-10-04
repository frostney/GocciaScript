/*---
description: Assigning to a const names it in the TypeError, and a const still in its dead zone throws the named ReferenceError instead
features: [const-declaration, temporal-dead-zone]
---*/

const messageOf = (run) => {
  try {
    run();
  } catch (error) {
    return `${error.constructor.name}: ${error.message}`;
  }
  return "no error";
};

const constant = (name) => `TypeError: Assignment to constant variable '${name}'`;
const tdz = (name) => `ReferenceError: Cannot access '${name}' before initialization`;

describe("const assignment error message", () => {
  test("assignment, update, compound and logical assignment name the const", () => {
    expect(messageOf(() => {
      const fixed = 1;
      fixed = 2;
    })).toBe(constant("fixed"));
    expect(messageOf(() => {
      const fixed = 1;
      fixed++;
    })).toBe(constant("fixed"));
    expect(messageOf(() => {
      const fixed = 1;
      fixed += 2;
    })).toBe(constant("fixed"));
    expect(messageOf(() => {
      const fixed = 0;
      fixed ||= 2;
    })).toBe(constant("fixed"));
    expect(messageOf(() => {
      const fixed = 1;
      {
        fixed = 3;
      }
    })).toBe(constant("fixed"));
  });

  test("destructuring and for-of targets name the const", () => {
    expect(messageOf(() => {
      const fixed = 1;
      [fixed] = [2];
    })).toBe(constant("fixed"));
    expect(messageOf(() => {
      const fixed = 1;
      ({ nested: [fixed] } = { nested: [2] });
    })).toBe(constant("fixed"));
    expect(messageOf(() => {
      const fixed = 1;
      for (fixed of [2]) {
      }
    })).toBe(constant("fixed"));
  });

  test("a const captured by a closure is named", () => {
    expect(messageOf(() => {
      const fixed = 1;
      (() => {
        fixed = 2;
      })();
    })).toBe(constant("fixed"));
    expect(messageOf(() => {
      const fixed = 1;
      (() => {
        ++fixed;
      })();
    })).toBe(constant("fixed"));
    expect(messageOf(() => {
      const fixed = null;
      (() => {
        fixed ??= 2;
      })();
    })).toBe(constant("fixed"));
    expect(messageOf(() => {
      const fixed = 1;
      (() => {
        ({ fixed } = { fixed: 2 });
      })();
    })).toBe(constant("fixed"));
  });

  test("a class binding is named when its own method assigns it", () => {
    expect(messageOf(() => {
      class Fixed {
        static reassign() {
          Fixed = 1;
        }
      }
      Fixed.reassign();
    })).toBe(constant("Fixed"));
  });

  test("a const in its dead zone throws the ReferenceError, not the TypeError", () => {
    expect(messageOf(() => {
      early = 1;
      const early = 0;
    })).toBe(tdz("early"));
    expect(messageOf(() => {
      const write = () => {
        early = 1;
      };
      write();
      const early = 0;
    })).toBe(tdz("early"));
    expect(messageOf(() => {
      const write = () => {
        [early] = [1];
      };
      write();
      const early = 0;
    })).toBe(tdz("early"));
    expect(messageOf(() => {
      class Self extends (Self = Object) {}
    })).toBe(tdz("Self"));
  });
});
