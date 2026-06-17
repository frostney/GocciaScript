/*---
description: Function.length and Function.name properties
features: [Function.length, Function.name]
---*/

describe("Function.length", () => {
  test("counts formal parameters", () => {
    const f0 = () => {};
    const f1 = (a) => a;
    const f2 = (a, b) => a + b;
    const f3 = (a, b, c) => a + b + c;

    expect(f0.length).toBe(0);
    expect(f1.length).toBe(1);
    expect(f2.length).toBe(2);
    expect(f3.length).toBe(3);
  });

  test("stops counting at default parameter", () => {
    const f = (a, b = 1, c) => {};
    expect(f.length).toBe(1);
  });

  test("stops counting at rest parameter", () => {
    const f = (a, b, ...rest) => {};
    expect(f.length).toBe(2);
  });

  test("deleted materialized length does not resurrect", () => {
    const f = (a, b) => {};

    Object.defineProperty(f, "length", {
      value: 10,
      configurable: true,
    });

    expect(f.length).toBe(10);
    expect(delete f.length).toBe(true);
    expect(Object.getOwnPropertyDescriptor(f, "length")).toBeUndefined();
  });
});

describe("Function.name", () => {
  test("variable-assigned arrow function infers name", () => {
    const add = (a, b) => a + b;
    expect(add.name).toBe("add");
  });

  test("let-assigned arrow function infers name", () => {
    let greet = () => "hello";
    expect(greet.name).toBe("greet");
  });

  test("object shorthand method has name", () => {
    const obj = { myMethod() { return 1; } };
    expect(obj.myMethod.name).toBe("myMethod");
  });

  test("object arrow property infers name", () => {
    const obj = { myArrow: () => 1 };
    expect(obj.myArrow.name).toBe("myArrow");
  });

  test("object getter has prefixed name", () => {
    const obj = { get myGetter() { return 1; } };
    const desc = Object.getOwnPropertyDescriptor(obj, "myGetter");
    expect(desc.get.name).toBe("get myGetter");
  });

  test("object setter has prefixed name", () => {
    const obj = { set mySetter(v) {} };
    const desc = Object.getOwnPropertyDescriptor(obj, "mySetter");
    expect(desc.set.name).toBe("set mySetter");
  });

  test("identifier assignment infers anonymous function names", () => {
    let arrow;
    let cls;

    arrow = () => {};
    cls = class {};

    expect(arrow.name).toBe("arrow");
    expect(cls.name).toBe("cls");
  });

  test("logical assignment infers names only when the right side is evaluated", () => {
    let andTarget = true;
    let orTarget = false;
    let nullishTarget = null;
    let keep = () => {};

    andTarget &&= () => {};
    orTarget ||= () => {};
    nullishTarget ??= class {};
    keep ||= class {};

    expect(andTarget.name).toBe("andTarget");
    expect(orTarget.name).toBe("orTarget");
    expect(nullishTarget.name).toBe("nullishTarget");
    expect(keep.name).toBe("keep");
  });

  test("member assignment does not infer anonymous function names", () => {
    const obj = {};

    obj.attr = class {};
    obj.arrow = () => {};

    expect(obj.attr.name).toBe("");
    expect(obj.arrow.name).toBe("");
  });

  test("deleted materialized name does not resurrect", () => {
    const f = (a) => {};

    Object.defineProperty(f, "name", {
      value: "renamed",
      configurable: true,
    });

    expect(f.name).toBe("renamed");
    expect(delete f.name).toBe(true);
    expect(Object.getOwnPropertyDescriptor(f, "name")).toBeUndefined();
  });
});

describe("Function restricted properties", () => {
  test("Function.prototype caller and arguments are throwing accessors", () => {
    const callerDesc = Object.getOwnPropertyDescriptor(Function.prototype, "caller");
    const argumentsDesc = Object.getOwnPropertyDescriptor(Function.prototype, "arguments");

    expect(callerDesc.enumerable).toBe(false);
    expect(callerDesc.configurable).toBe(true);
    expect(typeof callerDesc.get).toBe("function");
    expect(callerDesc.get).toBe(callerDesc.set);

    expect(argumentsDesc.enumerable).toBe(false);
    expect(argumentsDesc.configurable).toBe(true);
    expect(typeof argumentsDesc.get).toBe("function");
    expect(argumentsDesc.get).toBe(argumentsDesc.set);
    expect(argumentsDesc.get).toBe(callerDesc.get);

    expect(() => { return Function.prototype.caller; }).toThrow(TypeError);
    expect(() => { Function.prototype.arguments = {}; }).toThrow(TypeError);
  });

  test("arrow, class, generator, and method functions inherit restricted properties", () => {
    const arrow = () => {};
    const cls = class {};
    const obj = {
      method() {},
      *gen() {},
      get value() { return 1; },
      set value(v) {},
    };
    const accessors = Object.getOwnPropertyDescriptor(obj, "value");
    const values = [arrow, cls, obj.method, obj.gen, accessors.get, accessors.set];

    for (const value of values) {
      expect(value.hasOwnProperty("caller")).toBe(false);
      expect(value.hasOwnProperty("arguments")).toBe(false);
      expect(() => { return value.caller; }).toThrow(TypeError);
      expect(() => { value.caller = {}; }).toThrow(TypeError);
      expect(() => { return value.arguments; }).toThrow(TypeError);
      expect(() => { value.arguments = {}; }).toThrow(TypeError);
    }
  });
});
