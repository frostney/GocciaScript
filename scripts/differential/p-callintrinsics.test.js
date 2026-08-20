// Calls to a member named `call`, `apply` or `bind` must run the function the
// receiver actually carries, not the `Function.prototype` intrinsic that shares
// the name. The interesting shapes all put the callee on a function object,
// where the intrinsic is the plausible alternative reading, and invoke it with
// the argument shape the intrinsic itself would accept.
//
// Bun gates: this is ECMAScript property lookup and call semantics, and the
// testing API is incidental. Vitest is skipped for the same reason as the other
// language suites. See the CLASSIFICATION entry in
// scripts/test-cli-differential.ts.

describe("user-defined call/apply/bind on function objects", () => {
  test("static class methods run instead of the intrinsics", () => {
    class Registry {
      static call(name, ...rest) {
        return `Registry.call(${name})[${rest.join(",")}]`;
      }

      static apply(name, list) {
        return `Registry.apply(${name})[${list.join(",")}]`;
      }

      static bind(name) {
        return `Registry.bind(${name})`;
      }
    }

    expect(Registry.call("a", 1, 2)).toBe("Registry.call(a)[1,2]");
    expect(Registry.apply("a", [1, 2])).toBe("Registry.apply(a)[1,2]");
    expect(Registry.bind("a")).toBe("Registry.bind(a)");
    expect(Registry.call.name).toBe("call");
    expect(Registry.apply).not.toBe(Function.prototype.apply);
  });

  test("own properties on a plain function run instead of the intrinsics", () => {
    const host = function () {
      return "host";
    };
    host.call = (...args) => `own call(${args.join(",")})`;
    host.apply = (thisArg, list) => `own apply(${thisArg},[${list.join(",")}])`;
    host.bind = (...args) => `own bind(${args.join(",")})`;

    expect(host.call("t", 1, 2)).toBe("own call(t,1,2)");
    expect(host.apply("t", [1, 2])).toBe("own apply(t,[1,2])");
    expect(host.bind("t", 1)).toBe("own bind(t,1)");
    expect(host()).toBe("host");
  });

  test("an inherited `call` from the function's prototype chain wins", () => {
    const behaviour = {
      call(tag) {
        return `inherited call(${tag})`;
      },
      apply(tag, list) {
        return `inherited apply(${tag},${list.length})`;
      },
    };
    const host = function () {
      return "host";
    };
    Object.setPrototypeOf(host, behaviour);

    expect(host.call("t")).toBe("inherited call(t)");
    expect(host.apply("t", [1, 2, 3])).toBe("inherited apply(t,3)");
  });

  test("a different built-in installed as `apply` keeps its own semantics", () => {
    const inner = function () {
      return `inner(thisIsArray=${Array.isArray(this)})`;
    };
    const host = function () {
      return "host";
    };
    host.apply = Reflect.apply;

    // Reflect.apply(target, thisArgument, argumentsList): the receiver `host` is
    // not involved at all, and the array in the third position is what
    // Function.prototype.apply would have consumed from the second.
    expect(host.apply(inner, [], [])).toBe("inner(thisIsArray=true)");
    expect(host.apply.name).toBe("apply");
    expect(host.apply).not.toBe(Function.prototype.apply);
  });

  test("arity variants of a user-defined call are all forwarded", () => {
    const host = function () {
      return "host";
    };
    host.call = (...args) => args.length;

    expect(host.call()).toBe(0);
    expect(host.call(1)).toBe(1);
    expect(host.call(1, 2)).toBe(2);
    expect(host.call(1, 2, 3)).toBe(3);
    expect(host.call(1, 2, 3, 4)).toBe(4);
    expect(host.call(...[1, 2, 3, 4, 5])).toBe(5);
  });

  test("user-defined call/apply on plain objects and instances", () => {
    const gadget = {
      tag: "gadget",
      call(x) {
        return `${this.tag}.call(${x})`;
      },
      apply(x, list) {
        return `${this.tag}.apply(${x},${list.length})`;
      },
    };
    expect(gadget.call(1)).toBe("gadget.call(1)");
    expect(gadget.apply(1, [2, 3])).toBe("gadget.apply(1,2)");

    class Dispatcher {
      constructor(tag) {
        this.tag = tag;
      }

      call(x) {
        return `${this.tag}.call(${x})`;
      }

      apply(x, list) {
        return `${this.tag}.apply(${x},${list.length})`;
      }
    }
    const dispatcher = new Dispatcher("d");
    expect(dispatcher.call(1)).toBe("d.call(1)");
    expect(dispatcher.apply(1, [2, 3])).toBe("d.apply(1,2)");
  });

  test("shadowing an intrinsic does not disturb the intrinsic itself", () => {
    const host = function (a, b) {
      return `${this.tag}:${a}:${b}`;
    };
    host.call = () => "shadowed";
    host.apply = () => "shadowed";

    expect(host.call(1)).toBe("shadowed");
    expect(Function.prototype.call.call(host, { tag: "t" }, 1, 2)).toBe("t:1:2");
    expect(Function.prototype.apply.call(host, { tag: "t" }, [1, 2])).toBe("t:1:2");
  });
});

describe("the Function.prototype intrinsics themselves", () => {
  const collect = function (...args) {
    return `${this === undefined ? "undefined" : this.tag}:${args.join(",")}`;
  };

  test("call forwards the this value and every argument", () => {
    const receiver = { tag: "r" };
    expect(collect.call(receiver)).toBe("r:");
    expect(collect.call(receiver, 1)).toBe("r:1");
    expect(collect.call(receiver, 1, 2)).toBe("r:1,2");
    expect(collect.call(receiver, 1, 2, 3)).toBe("r:1,2,3");
    expect(collect.call(receiver, 1, 2, 3, 4)).toBe("r:1,2,3,4");
    expect(collect.call(...[receiver, 1, 2])).toBe("r:1,2");
  });

  test("apply forwards array arguments of every length", () => {
    const receiver = { tag: "r" };
    expect(collect.apply(receiver, [])).toBe("r:");
    expect(collect.apply(receiver, [1])).toBe("r:1");
    expect(collect.apply(receiver, [1, 2])).toBe("r:1,2");
    expect(collect.apply(receiver, [1, 2, 3])).toBe("r:1,2,3");
    expect(collect.apply(receiver, [1, 2, 3, 4])).toBe("r:1,2,3,4");
  });

  test("apply accepts array-likes and absent argument lists", () => {
    const receiver = { tag: "r" };
    expect(collect.apply(receiver, { 0: "x", 1: "y", length: 2 })).toBe("r:x,y");
    expect(collect.apply(receiver)).toBe("r:");
    expect(collect.apply(receiver, null)).toBe("r:");
  });

  test("bind fixes the this value and partially applies", () => {
    const receiver = { tag: "r" };
    expect(collect.bind(receiver)()).toBe("r:");
    expect(collect.bind(receiver, 1)(2)).toBe("r:1,2");
    expect(collect.bind(receiver, 1, 2)(3)).toBe("r:1,2,3");
    expect(typeof collect.bind(receiver)).toBe("function");
    expect(collect.bind(receiver)).not.toBe(collect);
  });

  test("methods reached through a class instance keep their receiver", () => {
    class Counter {
      constructor() {
        this.count = 7;
      }

      read(offset) {
        return this.count + offset;
      }
    }
    const counter = new Counter();
    expect(counter.read.call(counter, 1)).toBe(8);
    expect(counter.read.apply(counter, [2])).toBe(9);
    expect(counter.read.bind(counter, 3)()).toBe(10);
    expect(counter.read.call({ count: 100 }, 1)).toBe(101);
  });

  test("the intrinsics work when detached from the function", () => {
    const call = Function.prototype.call;
    const apply = Function.prototype.apply;
    const bind = Function.prototype.bind;
    const receiver = { tag: "r" };

    expect(call.call(collect, receiver, 1)).toBe("r:1");
    expect(apply.call(collect, receiver, [1, 2])).toBe("r:1,2");
    expect(bind.call(collect, receiver, 1)(2)).toBe("r:1,2");
    expect(call.apply(collect, [receiver, 1, 2, 3])).toBe("r:1,2,3");
  });

  // CreateListFromArrayLike reads every index of the argument array with Get,
  // so an elision is an absent property that resolves to undefined. A hole must
  // never reach the callee as a distinguishable value, in any argument count and
  // through any of the entry points that build the list.
  test("apply turns argument-array holes into undefined", () => {
    const args = function (...rest) {
      return rest.map((value) => String(value)).join("|");
    };

    expect(args.apply(undefined, [1, , 3])).toBe("1|undefined|3");
    expect(args.apply(undefined, [, 2, 3])).toBe("undefined|2|3");
    expect(args.apply(undefined, [1, 2, ,])).toBe("1|2|undefined");
    expect(args.apply(undefined, [, , ,])).toBe("undefined|undefined|undefined");
    expect(args.apply(undefined, [,])).toBe("undefined");
    expect(args.apply(undefined, [, ,])).toBe("undefined|undefined");
    expect(args.apply(undefined, [1, , , 4])).toBe("1|undefined|undefined|4");
    expect(args.apply(undefined, [1, , 3, , 5])).toBe("1|undefined|3|undefined|5");
    expect(((...rest) => rest.length).apply(undefined, [,])).toBe(1);
  });

  test("holes stay undefined through bound functions and detached apply", () => {
    const args = function (...rest) {
      return rest.map((value) => String(value)).join("|");
    };
    const apply = Function.prototype.apply;

    expect(args.bind(undefined).apply(undefined, [1, , 3])).toBe("1|undefined|3");
    expect(args.bind(undefined, 0).apply(undefined, [, 2])).toBe("0|undefined|2");
    expect(apply.call(args, undefined, [1, , 3])).toBe("1|undefined|3");
    expect(Reflect.apply(args, undefined, [1, , 3])).toBe("1|undefined|3");
    expect(args(...[1, , 3])).toBe("1|undefined|3");
    expect(((a, b, c) => b === undefined).apply(undefined, [1, , 3])).toBe(true);
  });

  test("argument-array holes are read through the prototype chain", () => {
    const args = function (...rest) {
      return rest.map((value) => String(value)).join("|");
    };
    let reads = 0;

    Object.defineProperty(Array.prototype, 1, {
      get() {
        reads += 1;
        return "inherited";
      },
      configurable: true,
    });

    try {
      expect(args.apply(undefined, [1, , 3])).toBe("1|inherited|3");
      expect(args.apply(undefined, [1, ,])).toBe("1|inherited");
      expect(args.apply(undefined, [1, , 3, 4])).toBe("1|inherited|3|4");
      expect(args.bind(undefined).apply(undefined, [1, , 3])).toBe("1|inherited|3");
      expect(Reflect.apply(args, undefined, [1, , 3])).toBe("1|inherited|3");
      expect(args(...[1, , 3])).toBe("1|inherited|3");
      expect(reads).toBe(6);
    } finally {
      delete Array.prototype[1];
    }
  });

  test("call and apply reject non-callable receivers", () => {
    const notCallable = { call: Function.prototype.call, apply: Function.prototype.apply };
    expect(() => notCallable.call(undefined)).toThrow(TypeError);
    expect(() => notCallable.apply(undefined, [])).toThrow(TypeError);
  });
});
