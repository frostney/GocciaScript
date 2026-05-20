/*---
description: Decorated computed fields use the resolved property key in context and initializer wiring
features: [decorators, class-fields, computed-property-names]
---*/

describe("computed field decorators", () => {
  test("string computed field context and initializer use resolved key", () => {
    let receivedName;
    const key = "x";
    const decorate = (value, context) => {
      receivedName = context.name;
      return (initialValue) => initialValue + 1;
    };

    class C {
      @decorate
      [key] = 41;
    }

    const instance = new C();
    expect(receivedName).toBe("x");
    expect(instance.x).toBe(42);
  });

  test("symbol computed field context and initializer preserve symbol key", () => {
    let receivedName;
    const key = Symbol("field");
    const decorate = (value, context) => {
      receivedName = context.name;
      return (initialValue) => initialValue + 1;
    };

    class C {
      @decorate
      [key] = 41;
    }

    const instance = new C();
    expect(receivedName === key).toBe(true);
    expect(instance[key]).toBe(42);
  });

  test("static computed field context uses resolved key", () => {
    let receivedName;
    const key = "staticName";
    const decorate = (value, context) => {
      receivedName = context.name;
    };

    class C {
      @decorate
      static [key] = 1;
    }

    expect(receivedName).toBe("staticName");
    expect(C.staticName).toBe(1);
  });
});
