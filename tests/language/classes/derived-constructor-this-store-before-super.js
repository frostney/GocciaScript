/*---
description: A store or compound assignment through this before super() throws ReferenceError before its value is evaluated
features: [class, class-inheritance]
---*/

describe("this.x = v and this.x op= v before super()", () => {
  test("throw ReferenceError without evaluating the value", () => {
    const log = [];
    const value = (label) => {
      log.push(label);
      return 1;
    };
    class Base {}
    class Store extends Base {
      constructor() {
        this.x = value("store");
        super();
      }
    }
    class Compound extends Base {
      constructor() {
        this.x += value("compound");
        super();
      }
    }
    class Nested extends Base {
      constructor() {
        this.a.b = value("nested");
        super();
      }
    }
    expect(() => new Store()).toThrow(ReferenceError);
    expect(() => new Compound()).toThrow(ReferenceError);
    expect(() => new Nested()).toThrow(ReferenceError);
    expect(log).toEqual([]);
  });

  test("work once super() has returned", () => {
    class Base {
      constructor() {
        this.count = 1;
      }
    }
    class Derived extends Base {
      constructor() {
        super();
        this.count += 2;
        this.label = "done";
        this.count *= 2;
      }
    }
    const instance = new Derived();
    expect(instance.count).toBe(6);
    expect(instance.label).toBe("done");
  });
});
