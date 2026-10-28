/*---
description: Re-evaluated classes create distinct private brands
features: [classes, private-fields, private-methods, private-static-fields]
---*/

test("private instance methods are branded per class evaluation", () => {
  const createInstance = () => {
    const C = class {
      #method() {
        return "ok";
      }

      access(receiver) {
        return receiver.#method();
      }
    };
    return new C();
  };

  const first = createInstance();
  const second = createInstance();

  expect(first.access(first)).toBe("ok");
  expect(second.access(second)).toBe("ok");
  expect(() => first.access(second)).toThrow(TypeError);
  expect(() => second.access(first)).toThrow(TypeError);
});

test("private instance accessors are branded per class evaluation", () => {
  const createInstance = () => {
    const C = class {
      #value = "ok";

      get #reader() {
        return this.#value;
      }

      set #writer(value) {
        this.#value = value;
      }

      read(receiver) {
        return receiver.#reader;
      }

      write(receiver, value) {
        receiver.#writer = value;
      }
    };
    return new C();
  };

  const first = createInstance();
  const second = createInstance();

  expect(first.read(first)).toBe("ok");
  first.write(first, "changed");
  expect(first.read(first)).toBe("changed");
  expect(() => first.read(second)).toThrow(TypeError);
  expect(() => first.write(second, "bad")).toThrow(TypeError);
});

test("private static fields are branded per class evaluation", () => {
  const createClass = () => class {
    static #value = "ok";

    static access() {
      return this.#value;
    }
  };

  const First = createClass();
  const Second = createClass();

  expect(First.access()).toBe("ok");
  expect(Second.access()).toBe("ok");
  expect(() => First.access.call(Second)).toThrow(TypeError);
  expect(() => Second.access.call(First)).toThrow(TypeError);
});

test("private static methods and accessors are branded per class evaluation", () => {
  const createClass = () => class {
    static #value = "ok";

    static #method() {
      return this.#value;
    }

    static get #reader() {
      return this.#value;
    }

    static methodAccess() {
      return this.#method();
    }

    static getterAccess() {
      return this.#reader;
    }
  };

  const First = createClass();
  const Second = createClass();

  expect(First.methodAccess()).toBe("ok");
  expect(Second.getterAccess()).toBe("ok");
  expect(() => First.methodAccess.call(Second)).toThrow(TypeError);
  expect(() => Second.getterAccess.call(First)).toThrow(TypeError);
});

test("static field arrows are branded per class evaluation", () => {
  const createClass = () =>
    class {
      #value = "ok";
      static read = (receiver) => receiver.#value;
      static has = (receiver) => #value in receiver;
    };

  const First = createClass();
  const Second = createClass();

  expect(First.read(new First())).toBe("ok");
  expect(First.has(new First())).toBe(true);
  expect(First.has(new Second())).toBe(false);
  expect(() => First.read(new Second())).toThrow(TypeError);
});

test("static computed methods are branded per class evaluation", () => {
  const key = "readStatic";
  const createClass = () =>
    class {
      #value = "ok";
      static [key](receiver) {
        return receiver.#value;
      }
    };

  const First = createClass();
  const Second = createClass();

  expect(First.readStatic(new First())).toBe("ok");
  expect(() => First.readStatic(new Second())).toThrow(TypeError);
});

describe("private names resolve through the class body the code is in", () => {
  // ES2026 §15.7.14 ClassDefinitionEvaluation gives each evaluation of a
  // class body fresh Private Names, and every function created inside the
  // body captures that PrivateEnvironment (§10.2.3 OrdinaryFunctionCreate).
  const key = "read";
  const symbol = Symbol("read");
  const createClass = () =>
    class {
      #value;
      static #shared = "shared";
      constructor(value) {
        this.#value = value;
      }
      [key](receiver) {
        return receiver.#value;
      }
      [symbol](receiver) {
        return receiver.#value;
      }
      get [key + "Getter"]() {
        return this.#value;
      }
      set [key + "Setter"](receiver) {
        receiver.#value = "written";
      }
      static literal() {
        return {
          read(receiver) {
            return receiver.#value;
          },
          write(receiver) {
            receiver.#value = "written";
            return receiver.#value;
          },
          has(receiver) {
            return #value in receiver;
          },
          get getter() {
            return this.receiver.#value;
          },
        };
      }
      static innerClass() {
        return class {
          read(receiver) {
            return receiver.#value;
          }
          has(receiver) {
            return #value in receiver;
          }
          readShared(receiver) {
            return receiver.#shared;
          }
        };
      }
    };

  test("computed instance methods are branded per class evaluation", () => {
    const First = createClass();
    const Second = createClass();
    const first = new First("first");
    const second = new Second("second");

    expect(first[key](new First("other"))).toBe("other");
    expect(first[symbol](new First("other"))).toBe("other");
    expect(() => first[key](second)).toThrow(TypeError);
    expect(() => first[symbol](second)).toThrow(TypeError);
  });

  test("computed instance accessors are branded per class evaluation", () => {
    const First = createClass();
    const Second = createClass();
    const second = new Second("second");
    const getter = Object.getOwnPropertyDescriptor(First.prototype, "readGetter").get;

    expect(getter.call(new First("first"))).toBe("first");
    expect(() => getter.call(second)).toThrow(TypeError);
    expect(() => {
      new First("first").readSetter = second;
    }).toThrow(TypeError);
    expect(second[key](second)).toBe("second");
  });

  test("object literal methods in a class body are branded per class evaluation", () => {
    const First = createClass();
    const Second = createClass();
    const second = new Second("second");
    const literal = First.literal();

    expect(literal.read(new First("first"))).toBe("first");
    expect(literal.has(new First("first"))).toBe(true);
    expect(() => literal.read(second)).toThrow(TypeError);
    expect(() => literal.write(second)).toThrow(TypeError);
    expect(literal.has(second)).toBe(false);
    literal.receiver = second;
    expect(() => literal.getter).toThrow(TypeError);
    expect(second[key](second)).toBe("second");
  });

  test("classes nested in a method are branded per outer class evaluation", () => {
    const First = createClass();
    const Second = createClass();
    const Inner = First.innerClass();

    expect(new Inner().read(new First("first"))).toBe("first");
    expect(new Inner().has(new First("first"))).toBe(true);
    expect(() => new Inner().read(new Second("second"))).toThrow(TypeError);
    expect(new Inner().has(new Second("second"))).toBe(false);
    expect(new Inner().readShared(First)).toBe("shared");
    expect(() => new Inner().readShared(Second)).toThrow(TypeError);
  });
});

describe("code in a class body outside its methods uses the class's private names", () => {
  // Static field initializers, static blocks and their nested functions all
  // run with the class's PrivateEnvironment (ES2026 §15.7.14 step 12), even
  // though they are evaluated while the class is being defined.
  const createClass = () =>
    class Owner {
      #value = "own";
      static literal = {
        read(receiver) {
          return receiver.#value;
        },
      };
      static arrows = [(receiver) => receiver.#value, (receiver) => #value in receiver];
      static Inner = class {
        read(receiver) {
          return receiver.#value;
        }
      };
      static fromBlock;
      static {
        Owner.fromBlock = (receiver) => receiver.#value;
      }
    };

  test("functions from static field initializers accept their own class's instances", () => {
    const First = createClass();
    const first = new First();

    expect(First.literal.read(first)).toBe("own");
    expect(First.arrows[0](first)).toBe("own");
    expect(First.arrows[1](first)).toBe(true);
    expect(new First.Inner().read(first)).toBe("own");
    expect(First.fromBlock(first)).toBe("own");
  });

  test("functions from static field initializers reject another evaluation's instances", () => {
    const First = createClass();
    const Second = createClass();
    const second = new Second();

    expect(() => First.literal.read(second)).toThrow(TypeError);
    expect(() => First.arrows[0](second)).toThrow(TypeError);
    expect(First.arrows[1](second)).toBe(false);
    expect(() => new First.Inner().read(second)).toThrow(TypeError);
    expect(() => First.fromBlock(second)).toThrow(TypeError);
  });
});


describe("re-evaluated classes sharing one receiver", () => {
  const make = (Base, value) => class extends Base {
    #value = value;
    #method() { return value; }
    get #accessor() { return this.#value; }
    set #accessor(next) { this.#value = next; }
    read() { return this.#value; }
    write(next) { this.#value = next; }
    method() { return this.#method(); }
    get value() { return this.#accessor; }
    set value(next) { this.#accessor = next; }
    has(receiver) { return #value in receiver; }
  };

  test("base and derived evaluations keep their own fields and methods", () => {
    const Base = make(class {}, 1);
    const Derived = make(Base, 2);
    const receiver = new Derived();
    expect(Base.prototype.read.call(receiver)).toBe(1);
    expect(receiver.read()).toBe(2);
    expect(Base.prototype.method.call(receiver)).toBe(1);
    expect(receiver.method()).toBe(2);
    Base.prototype.write.call(receiver, 3);
    expect(Base.prototype.read.call(receiver)).toBe(3);
    expect(receiver.read()).toBe(2);
    expect(Base.prototype.has(receiver)).toBe(true);
    expect(Derived.prototype.has(receiver)).toBe(true);
    expect(Derived.prototype.has(new Base())).toBe(false);
    expect(() => Derived.prototype.read.call(new Base())).toThrow(TypeError);
  });

  test("base and derived evaluations keep their own accessors", () => {
    const Base = make(class {}, 1);
    const Derived = make(Base, 2);
    const receiver = new Derived();
    const access = Object.getOwnPropertyDescriptor(Base.prototype, "value");
    expect(access.get.call(receiver)).toBe(1);
    expect(receiver.value).toBe(2);
    access.set.call(receiver, 4);
    expect(access.get.call(receiver)).toBe(4);
    expect(receiver.value).toBe(2);
  });

  test("return overrides keep private elements from both evaluations", () => {
    class ReturnReceiver {
      constructor(receiver) { return receiver; }
    }
    const First = make(ReturnReceiver, 1);
    const Second = make(ReturnReceiver, 2);
    const receiver = {};
    new First(receiver);
    new Second(receiver);
    expect(First.prototype.read.call(receiver)).toBe(1);
    expect(Second.prototype.read.call(receiver)).toBe(2);
    expect(First.prototype.method.call(receiver)).toBe(1);
    expect(Second.prototype.method.call(receiver)).toBe(2);
    First.prototype.write.call(receiver, 5);
    expect(First.prototype.read.call(receiver)).toBe(5);
    expect(Second.prototype.read.call(receiver)).toBe(2);
    expect(() => new First(receiver)).toThrow(TypeError);
  });
});


test("discarded classes do not give new classes access to their receiver", () => {
  const make = () => class extends class {
    constructor(receiver) { return receiver; }
  } {
    #value = 1;
    static has(receiver) { return #value in receiver; }
  };
  const receiver = {};
  new (make())(receiver);
  const aliased = Array.from({ length: 20000 }, () => 0).some(() => {
    const Candidate = make();
    // Allocation pressure allows the discarded class to be collected.
    Array.from({ length: 50 }, () => ({ value: 1 }));
    return Candidate.has(receiver);
  });
  expect(aliased).toBe(false);
});
