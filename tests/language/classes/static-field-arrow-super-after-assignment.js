/*---
description: Assigning a static field's arrow function to a class property does not change the home object its super references use
features: [static-properties, class-fields]
---*/

// ES2026 §10.2.7 MakeMethod sets [[HomeObject]] only when a method is defined.
// An arrow in a static field initializer uses the initializer's home object,
// the class itself, so super resolves to the parent class. Storing the same
// arrow on the class first is an ordinary assignment and must not give it the
// class prototype as home object.

const makeBase = (log) =>
  class Base {
    static get count() {
      return 10;
    }

    static set count(value) {
      log.push(`static set ${value}`);
    }

    get count() {
      return 20;
    }

    set count(value) {
      log.push(`prototype set ${value}`);
    }
  };

test("super assignment in a static field arrow stored with a dot assignment reaches the parent class", () => {
  const log = [];
  const Base = makeBase(log);
  class Derived extends Base {
    static update = (Derived.stored = () => {
      super.count = 1;
    });
  }

  Derived.stored();
  Derived.update();

  expect(Derived.stored).toBe(Derived.update);
  expect(log).toEqual(["static set 1", "static set 1"]);
});

test("super assignment in a static field arrow stored with a computed assignment reaches the parent class", () => {
  const log = [];
  const Base = makeBase(log);
  const key = "stored";
  class Derived extends Base {
    static update = (Derived[key] = () => {
      super.count = 2;
    });
  }

  Derived[key]();

  expect(log).toEqual(["static set 2"]);
});

test("super compound assignment in a stored static field arrow reads and writes the parent class", () => {
  const log = [];
  const Base = makeBase(log);
  class Derived extends Base {
    static update = (Derived.stored = () => {
      super.count += 1;
    });
  }

  Derived.update();

  expect(log).toEqual(["static set 11"]);
});
