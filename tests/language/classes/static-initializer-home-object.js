/*---
description: Static field initializers and static blocks are methods whose home object is the class, so super reads the class's prototype chain
features: [static-properties, class-fields, class-static-block]
---*/

// ES2026 §15.7.10 ClassFieldDefinitionEvaluation and §15.7.11
// ClassStaticBlockDefinitionEvaluation call MakeMethod with the class itself
// as the home object. `super.x` there is a lookup on the class's
// [[Prototype]]: Function.prototype for a class without `extends`, the parent
// class otherwise. Arrows inside the initializer share its home object.

describe("static initializer home object", () => {
  test("super in a static field initializer of a base class reads Function.prototype", () => {
    class C {
      static direct = super.call;
      static arrow = () => super.call;
      static nested = [() => super.call];
    }
    expect(C.direct).toBe(Function.prototype.call);
    expect(C.arrow()).toBe(Function.prototype.call);
    expect(C.nested[0]()).toBe(Function.prototype.call);
  });

  test("super in a computed static field's arrow of a base class reads Function.prototype", () => {
    const key = "computed";
    class C {
      static [key] = () => super.call;
    }
    expect(C[key]()).toBe(Function.prototype.call);
  });

  test("super in a static block of a base class reads Function.prototype", () => {
    let direct;
    let arrow;
    class C {
      static {
        direct = super.call;
        arrow = () => super.call;
      }
    }
    expect(direct).toBe(Function.prototype.call);
    expect(arrow()).toBe(Function.prototype.call);
  });

  test("super in a static field initializer of a derived class reads the parent class", () => {
    const key = "computed";
    class Base {
      static describe() {
        return `static ${this.name}`;
      }

      describe() {
        return "prototype";
      }
    }
    class Derived extends Base {
      static direct = super.describe();
      static arrow = () => super.describe();
      static nested = [() => super.describe()];
      static [key] = () => super.describe();
      static #hidden = super.describe();
      static hidden = () => Derived.#hidden;
    }
    expect(Derived.direct).toBe("static Derived");
    expect(Derived.arrow()).toBe("static Derived");
    expect(Derived.nested[0]()).toBe("static Derived");
    expect(Derived[key]()).toBe("static Derived");
    expect(Derived.hidden()).toBe("static Derived");
  });

  test("a static field initializer still sees the class binding, this and earlier fields", () => {
    class C {
      static first = 1;
      static second = this.first + 1;
      static third = C.second + 1;
      static self = () => this;
    }
    expect(C.second).toBe(2);
    expect(C.third).toBe(3);
    expect(C.self()).toBe(C);
  });

  test("an exception thrown by a static field initializer stops the class definition", () => {
    const order = [];
    expect(() => {
      class C {
        static a = order.push("a");
        static b = (() => {
          throw new RangeError("stop");
        })();
        static c = order.push("c");
      }
    }).toThrow(RangeError);
    expect(order).toEqual(["a"]);
  });
});
