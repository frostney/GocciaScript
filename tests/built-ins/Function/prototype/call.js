/*---
description: Function.prototype.call
features: [Function]
---*/

describe("Function.prototype.call", () => {
  test("calls a function with given this and arguments", () => {
    const fn = (a, b) => a + b;
    expect(fn.call(undefined, 1, 2)).toBe(3);
  });

  test("passes thisArg correctly", () => {
    class Obj {
      constructor(x) { this.x = x; }
      getX() { return this.x; }
    }
    const obj = new Obj(42);
    expect(obj.getX.call(obj)).toBe(42);
  });

  test("works with no arguments", () => {
    const fn = () => "hello";
    expect(fn.call(undefined)).toBe("hello");
  });

  test("accessible via Function.prototype.call", () => {
    expect(typeof Function.prototype.call).toBe("function");
  });

  test("throws TypeError when this value is not callable", () => {
    const call = Function.prototype.call;

    expect(() => call.call({})).toThrow(TypeError);
    expect(() => call.call(0)).toThrow(TypeError);
  });

  test("Function.prototype is a callable no-op", () => {
    expect(typeof Function.prototype).toBe("function");
    expect(Function.prototype()).toBe(undefined);
    expect(Function.prototype.call({ ignored: true }, 1, 2)).toBe(undefined);
    expect(Function.prototype.apply({ ignored: true }, [1, 2])).toBe(undefined);
    expect(Function.prototype.bind({ ignored: true }, 1)()).toBe(undefined);
  });

  test("has correct name and length", () => {
    expect(Function.prototype.call.name).toBe("call");
    expect(Function.prototype.call.length).toBe(1);
  });

  test("works on class constructors as type conversion", () => {
    expect(Number.call(null, 42)).toBe(42);
    expect(Number.call(null, "3.14")).toBe(3.14);
    expect(Number.call(null)).toBe(0);
    expect(String.call(null, 42)).toBe("42");
    expect(String.call(null, true)).toBe("true");
    expect(String.call(null)).toBe("");
    expect(Boolean.call(null, 1)).toBe(true);
    expect(Boolean.call(null, 0)).toBe(false);
  });

  test("forwards every argument count", () => {
    const collect = ({
      m(...args) {
        return `${this.tag}:${args.join(",")}`;
      },
    }).m;
    const receiver = { tag: "r" };

    expect(collect.call(receiver)).toBe("r:");
    expect(collect.call(receiver, 1)).toBe("r:1");
    expect(collect.call(receiver, 1, 2)).toBe("r:1,2");
    expect(collect.call(receiver, 1, 2, 3)).toBe("r:1,2,3");
    expect(collect.call(receiver, 1, 2, 3, 4)).toBe("r:1,2,3,4");
    expect(collect.call(...[receiver, 1, 2])).toBe("r:1,2");
  });

  test("an own call property is invoked instead of the intrinsic", () => {
    const host = () => "host";
    host.call = (...args) => `own call(${args.join(",")})`;

    expect(host.call()).toBe("own call()");
    expect(host.call("t")).toBe("own call(t)");
    expect(host.call("t", 1, 2)).toBe("own call(t,1,2)");
    expect(host()).toBe("host");
    expect(Function.prototype.call.call(host, undefined)).toBe("host");
  });

  test("a call inherited from the function's prototype chain is invoked", () => {
    const behaviour = {
      call(tag) {
        return `inherited call(${tag})`;
      },
    };
    const host = () => "host";
    Object.setPrototypeOf(host, behaviour);

    expect(host.call("t")).toBe("inherited call(t)");
  });

  test("a class static named call is the class's own method", () => {
    class Registry {
      static call(name, ...rest) {
        return `Registry.call(${name})[${rest.join(",")}]`;
      }
    }

    expect(Registry.call("a")).toBe("Registry.call(a)[]");
    expect(Registry.call("a", 1, 2)).toBe("Registry.call(a)[1,2]");
    expect(Registry.call).not.toBe(Function.prototype.call);
  });
});
