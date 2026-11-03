/*---
description: >
  A finally block that runs a loop after a catch clause's binding pattern
  failed to destructure the exception (ES2026 §14.15.3 try statement,
  §8.6.2 BindingInitialization) must not disturb the failure it will rethrow:
  the loop's exit clears dead registers, and the register holding the pending
  exception is not one of them.
features: [try-catch, destructuring, while-loops]
---*/

describe("a loop in a finally block", () => {
  test("keeps the TypeError a catch pattern raised", () => {
    const run = () => {
      let count = 0;
      try {
        try {
          throw null;
        } catch ({ value }) {
          count = 1;
        } finally {
          do {
            break;
          } while (count++ < 3);
        }
      } catch (error) {
        return [error.constructor.name, count];
      }
      return ["no throw", count];
    };
    expect(run()).toEqual(["TypeError", 0]);
  });

  test("keeps an exception a catch clause rethrows past its finally block", () => {
    const run = () => {
      try {
        try {
          throw new RangeError("original");
        } catch (error) {
          throw error;
        } finally {
          let spins = 0;
          while (spins < 2) {
            spins++;
          }
        }
      } catch (error) {
        return [error.constructor.name, error.message];
      }
      return ["no throw", ""];
    };
    expect(run()).toEqual(["RangeError", "original"]);
  });

  test("keeps the exception a non-matching catch pattern rethrows", () => {
    const run = () => {
      try {
        try {
          throw { code: 1 };
        } catch (error is { code: 2 }) {
          return "matched";
        } finally {
          let spins = 0;
          while (spins < 2) {
            spins++;
          }
        }
      } catch (outer) {
        return outer.code;
      }
      return "no throw";
    };
    expect(run()).toBe(1);
  });
});
