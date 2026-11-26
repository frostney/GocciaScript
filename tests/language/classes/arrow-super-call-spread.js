/*---
description: super(...) with spread arguments inside an arrow in a derived constructor calls the parent with the spread values
features: [class, arrow-function, spread]
---*/

// ES2026 13.3.7.1 SuperCall: ArgumentListEvaluation of the arguments, spread
// included, and an arrow's super() reaches its enclosing derived constructor.
class Base {
  constructor(...args) {
    this.args = args;
  }
}

describe("super(...spread) in an arrow", () => {
  test("passes the spread arguments and the ones around them", () => {
    class Derived extends Base {
      constructor(...rest) {
        const callSuper = () => super(0, ...rest, "end");
        callSuper();
      }
    }
    expect(new Derived(1, 2).args).toEqual([0, 1, 2, "end"]);
  });

  test("an empty spread passes no arguments, and a hole passes undefined", () => {
    class Empty extends Base {
      constructor() {
        const callSuper = () => super(...[]);
        callSuper();
      }
    }
    class Holes extends Base {
      constructor() {
        const callSuper = () => super(...[7, , 9]);
        callSuper();
      }
    }
    expect(new Empty().args).toEqual([]);
    expect(new Holes().args).toEqual([7, undefined, 9]);
  });

  test("a nested arrow binds this, and a second call is a ReferenceError", () => {
    let second;
    class Twice extends Base {
      constructor() {
        const outer = () => () => super(...["x"]);
        outer()();
        try {
          outer()();
        } catch (error) {
          second = error;
        }
      }
    }
    expect(new Twice().args).toEqual(["x"]);
    expect(second instanceof ReferenceError).toBe(true);
  });
});
